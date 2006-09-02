unit LabyrintheUtils;

interface

uses
  Classes, Windows, SysUtils, Dialogs, Forms, Graphics, ScUtils, ScLists,
  ScStrUtils, ScExtra, StrUtils;

type
  TFleche = (SansDirection, Haut, Droite, Bas, Gauche);
  TSensRotation = (Direct, Indirect);
  TStyleBouton = (sPoussoir, sCommutateur, sBorneInfo, sCache, sPerso, sObjet, sEpreuve, sDirection, sCommutateurOn, sCommutateurOff, sTransporteur, sPasBouton);
  T45_StyleBouton = array [1..45] of TStyleBouton;
  EInvalidAction = class(Exception);
  EIsSauvegarde = class(Exception);

  TDimensions = record
    Lignes, Colonnes, Etages : integer;
  end;

  {TInfosJeu = record
    Bouees, Planches : integer;
    ClesArgent, ClesOr, Barque : integer;
    Indices : boolean;
  end; }

  TFileInfos = record
    PeutEditer : boolean;
    Version : string;
    Sauvegarde : boolean;
  end;

  TPosition = record
    LaCase : T3DPoint;
    LaPosition : TPoint;
  end;

  TLabyrinthe = class;

  TCaseActive = class
  private
    Labyrinthe : TLabyrinthe;
    Actions : TScStrings;
    StrIci : string;
    //Infos : TInfosJeu;
    Position : T3DPoint;
    EstAlleARien : boolean;
    procedure SetActions(Acts : TScStrings);
    procedure RemplaceCase(var Str : string);
    function NbreSubStr(SubStr, Str : string) : integer;
    function Recherche(SubStr, Str : string) : integer;
    function RechercheTousMots(SubStr, Str : string) : integer;
    procedure RemplaceToutesVariables(var Str : string);
    function GetXWord(Str : string; X : integer) : string;
    function StrToIntSpec(Str : string; Negatifs : boolean) : integer;
    function EvalBool(StrBool : string) : boolean;
    function EvalUnBool(Str : string) : boolean;
    procedure RemplaceChaine (var Str : string; Chaine1, Chaine2 : string);
    procedure RemplaceMot(var Str : string; Mot, Chaine : string);
    procedure RemplaceTousMots(var Str : string; Mot, Chaine : string);
    procedure RemplaceSi(var Str : string);
    function VerifieBonsChar(Str : string) : boolean;
    function VerifieBonMessage(var Str : string) : boolean;
    function CharSuivant(C : Char; S : string) : Char;

    procedure ExecuteRemplacer   (Str : string);
    procedure ExecuteConvertir   (Str : string);
    procedure ExecuteDeplacer    (Str : string);
    procedure ExecuteIncr        (Str : string; Incr : integer);
    procedure ExecuteMult        (Str : string);
    function  ExecuteInformation (TitreInfo, Str : string; mb : integer) : boolean;
    procedure ExecuteChoix       (Str : string);
    procedure ExecuteSon         (Str : string);
    procedure ExecuteIndice      (Str : string);
    function  ExecuteEchec       (Str : string) : boolean;
    procedure ExecuteImpasse     (Str : string);
    function  ExecuteGagne       (Str : string) : boolean;
    function  ExecuteAllerA      (Str : string) : boolean;
    function  ExecuteMessage     (Str : string) : boolean;
    procedure ExecuteDescription;
  protected
    CaseDesactiver : Char;
  public
    Compteur : integer;
    constructor Create(Owner : TLabyrinthe);
    destructor Destroy; override;
    function Execute(var Ici : T3DPoint; var ABouge : boolean) : boolean;
    procedure AllerA(Str : string; var Ici : T3DPoint);
    property LesActions : TScStrings read Actions write SetActions;
  end;

  TCaseBouton = class (TCaseActive)
  private
    FStyle : TStyleBouton;
    FNom : string;
    procedure SetStyle(NewStyle : TStyleBouton);
    procedure SetNom(New : string);
  public
    Image : TBitmap;
    constructor Create(Owner : TLabyrinthe);
    destructor Destroy; override;
    property Style : TStyleBouton read FStyle write SetStyle;
    property Nom : string read FNom write SetNom;
  end;

  TCaseTelepor = class (TCaseActive)
  private
    FStyle : TStyleBouton;
    FNom : string;
    procedure SetStyle(NewStyle : TStyleBouton);
    procedure SetNom(NewNom : string);
  public
    Image : TBitmap;
    constructor Create(Owner : TLabyrinthe);
    destructor Destroy; override;
    property Style : TStyleBouton read FStyle write SetStyle;
    property Nom : string read FNom write SetNom;
  end;

  TCaseFin = class (TCaseActive)
  public
    constructor Create(Owner : TLabyrinthe);
  end;

  TActionsZone = class (TCaseActive)
  public
    constructor Create(Owner : TLabyrinthe);
  end;

  TActionsDebut = class (TCaseActive);

  T45_TCaseBouton  = array [1..45] of TCaseBouton;
  T30_TCaseTelepor = array [1..30] of TCaseTelepor;

  TLabyrinthe = class
  private
    FFileName : string;
    FEntete : TStrings;
    FConstruction : boolean;
    FNouveau : boolean;
    FLabyrinthe, SLEnteteBoutons, SLEnteteTeleporteurs : TScStrings;
    FDimensions : TDimensions;
    FFileInfos : TFileInfos;
    //FInfosJeu : TInfosJeu;
    FPosition : TPosition;
    FDescription : TStringList;
    FBoutons : T45_TCaseBouton;
    FTeleporteurs : T30_TCaseTelepor;
    FActionsFin : TCaseFin;
    FActionsDebut : TActionsDebut;
    FActionsZone : TStringList;
    ListeActionsZone : TScStrings;
    FNomLab : string;
    //FNomCases : string;
    procedure Init;
    procedure LoadActions(LesActions : TStringList);
    procedure SetLabyrinthe(X, Y, Z, SorteCase : integer);
    function GetLabyrinthe(X, Y, Z : integer) : integer;
    function Exterieur(X, Y, Z : integer; bordinclus : boolean) : boolean;
    function GetActionsZone(X, Y, Z : integer) : TActionsZone;
    function GetTouche : TFleche;
    property ActionsZone[X, Y, Z : integer] : TActionsZone read GetActionsZone;
  public
    Variables : array[1..20] of integer;
    Reponse, Reussite, Etape : integer;   //4.2.2
    AutorisePlanche : boolean;
    Attendu : boolean;
    ActiveCaseSuivante, Continuer : boolean;
    constructor CreateOpen(FileName : string);
    constructor CreateNew(Colonnes, Lignes, Etages : integer; Perso : boolean);
    constructor CreateActions(FileName : string; StringList : TStrings);
    destructor Destroy; override;
    procedure Sauvegarder(FileName : string; X, Y, Etage, B, P, PC, GC, Barque : integer; Indices : boolean);
    procedure Enregistrer(FileName : string);
    procedure EnregistrerActions(FileName : string);
    function ExecuteBouton(ID : integer; var Ici : T3DPoint) : boolean;
    function ExecuteTeleporteur(ID : integer; var Ici : T3DPoint) : boolean;
    procedure MessageFin(var Ici : T3DPoint; TypeFin : integer);
    procedure ExecuteChangeDeZone(var Ici : T3DPoint; X, Y, Z : integer);
    function ActionsZoneExists(X, Y, Z : integer) : boolean;
    function StyleBouton(Code : integer) : TStyleBouton;
    function CodeToImage(Code : integer) : TBitmap;
    procedure Poursuivre(B : boolean);
    function DefinitStyle(Defaut : TStyleBouton; Str : String) : TStyleBouton;
    function StyleToStr(St : TStyleBouton) : String;
    property FileName : string read FFileName;
    property Labyrinthe[X, Y, Z : integer] : integer read GetLabyrinthe write SetLabyrinthe; default;
    property NomLab : string read FNomLab write FNomLab;
    property NouveauNom : boolean write FNouveau;
    property Touche : TFleche read GetTouche;
    property Dimensions : TDimensions read FDimensions;
    property Description : TStringList read FDescription;
    property FileInfos : TFileInfos read FFileInfos;
   // property InfosJeu : TInfosJeu read FInfosJeu;
    property Position : TPosition read FPosition;
    property Boutons : T45_TCaseBouton read FBoutons;
    property Teleporteurs : T30_TCaseTelepor read FTeleporteurs;
    property ActionsFin : TCaseFin read FActionsFin;
    property ActionsDebut : TActionsDebut read FActionsDebut;
  end;

const
  clInvisible = clNone;
  maxbord = 15;
  VersionInterpreteur = 45;
  VersionEditeur = 'Version: 4.5';

  Herbe       = 48;
  Eau         = 49;
  Mur         = 50;
  Trou        = 51;
  BlocArgent  = 52;
  BlocOr      = 53;
  Nord        = 54;
  Est         = 55;
  Sud         = 56;
  Ouest       = 57;
  Carrefour   = 58;
  Tresor      = 59;
  FauxMur     = 63;
  MoulinDirect   = 224;
  MoulinIndirect = 225;
  Bouton = [33..47, 161..190];
  BoutonEnfonce = 64;
  TeleporteurI = 96;
  TeleporteurS = [97..109];
  TeleporteurP = [110..122];
  TeleporteurA = [123..126];
  Escalier     = [71..90];
  EscalierDescendant = 60;
  Ascenseur          = 61;
  AscenseurOuvert    = 5;
  EscalierMontant    = 62;
  Depart             = 65;
  Vide               = 66;
  Ciel =               226;
  Barque             = [193..202];

  Bouee       = 67;
  Planche     = 68;
  CleArgent   = 69;
  CleOr       = 70;

  EscalierM = 1;
  EscalierD = 2;
  Teleporteur = TeleporteurS+TeleporteurP+TeleporteurA;

  StylesPerso : set of TStyleBouton = [sPerso, sEpreuve, sObjet, sDirection];

var
  Bord : array [0..maxbord] of integer;
  TestActions : boolean = False;
  Direction, AncDirection : TFleche;
  AGagne, Indices, TouchePressee : boolean;
  Bouees, Planches, ClesArgent, ClesOr, NoBarque : integer;
  CouleurPion : TColor;
  Temporisation : integer;

  FNomCases : String;
  Cases : TBitmap;

procedure FixeInt(Src : TScStrings; Name : string; var Value : integer);
procedure FixeBool(Src : TScStrings; Name : string; var Value : boolean);
procedure FixeStr(Src : TScStrings; Name : string; var Value : string);

procedure RemplaceVariable(var Str : string; Variable, Valeur : string);

function CodeToNoBouton(CodeCase : integer) : integer;
function Div7(X : integer) : integer;
function Mod7(X : integer) : integer;

function VerifieIntPos(Str : string) : boolean;

function CaseRect(CodeCase : integer; StyleBouton : TStyleBouton) : TRect;

implementation

///////////////////////////////////////
/// Procédures et foncions globales ///
///////////////////////////////////////

procedure FixeInt(Src : TScStrings; Name : string; var Value : integer);
var Index : integer;
begin
  Index := Src.FindAtPos(Name+':');
  if Index <> -1 then
    Value := StrToIntDef(GetLastToken(Src[Index], ' '), -1);
end;

procedure FixeBool(Src : TScStrings; Name : string; var Value : boolean);
var Index : integer;
begin
  Index := Src.FindAtPos(Name+':');
  if Index <> -1 then
    Value := StrToBool(GetLastToken(Src[Index], ' '));
end;

procedure FixeStr(Src : TScStrings; Name : string; var Value : string);
var Index, Len : integer;
begin
  Index := Src.FindAtPos(Name+':');
  if Index = -1 then exit;
  Len := Length(GetFirstToken(Src[Index], ' '));
  Value := Copy(Src[Index], Len + 2, Length(Src[Index]));
end;

function Div7(X : integer) : integer;
begin
  if (X >= 0) or (X mod 7 = 0)
    then Result := X div 7
    else Result := X div 7 - 1;
end;

function Mod7(X : integer) : integer;
begin
  if (X >= 0) or (X mod 7 = 0)
    then Result := X mod 7
    else Result := X mod 7 + 7;
end;

function VerifieIntPos(Str : string) : boolean;
begin
  Result := StrToIntDef(Str, -1) >= 0;
end;

procedure RemplaceVariable(var Str : string; Variable, Valeur : string);
var I : integer;
begin
  while True do
  begin
    I := Pos(Variable, Str);
    if I = 0 then Break;
    Delete(Str, I, Length(Variable));
    Insert(Valeur, Str, I);
  end;
end;

function EvalCouleur(Str : string) : TColor;
var I : integer;
begin
  I := StrToIntDef(Str, -1);
  if I <> -1 then result := I
  else if Str = 'Rouge' then result := clRed
  else if Str = 'Vert'  then result := clLime
  else if Str = 'Jaune' then result := clYellow
  else if Str = 'Noir'  then result := clBlack
  else if Str = 'Blanc' then result := clWhite
  else if Str = 'Invisible' then result := clInvisible
  else result := clBlue
end;

function CaseRect(CodeCase : integer; StyleBouton : TStyleBouton) : TRect;
begin
  case CodeCase of
    Herbe, Depart  : Result := Rect(0  , 0  , 0, 0);
    Eau            : Result := Rect(30 , 0  , 0, 0);
    Mur, FauxMur   : Result := Rect(60 , 0  , 0, 0);
    Trou           : Result := Rect(90 , 0  , 0, 0);
    BlocArgent     : Result := Rect(120, 0  , 0, 0);
    BlocOr         : Result := Rect(0  , 30 , 0, 0);
    Nord           : Result := Rect(30 , 30 , 0, 0);
    Est            : Result := Rect(60 , 30 , 0, 0);
    Sud            : Result := Rect(90 , 30 , 0, 0);
    Ouest          : Result := Rect(120, 30 , 0, 0);
    Carrefour      : Result := Rect(60 , 150, 0, 0);
    MoulinDirect   : Result := Rect(90 , 150, 0, 0);
    MoulinIndirect : Result := Rect(120, 150, 0, 0);
    3, 96          : Result := Rect(0  , 60 , 0, 0);
    EscalierM, EscalierMontant, 71..90 : Result := Rect(30 , 60, 0, 0);
    EscalierD, EscalierDescendant      : Result := Rect(60 , 60, 0, 0);
    Ascenseur       : Result := Rect(30 , 120, 0, 0);
    AscenseurOuvert : Result := Rect(60 , 120, 0, 0);
    4               : Result := Rect(90 , 60 , 0, 0);
    BoutonEnfonce   : Result := Rect(120, 60 , 0, 0);
    Vide            : Result := Rect(0  , 90 , 0, 0);
    Tresor          : Result := Rect(30 ,150 , 0, 0);
    Bouee           : Result := Rect(30 , 90 , 0, 0);
    Planche         : Result := Rect(60 , 90 , 0, 0);
    CleArgent       : Result := Rect(90 , 90 , 0, 0);
    CleOr           : Result := Rect(120, 90 , 0, 0);
    193..202        : Result := Rect(0,   150, 0, 0);
    33..47, 97..126, 161..190 :
    begin
      case StyleBouton of
        sPoussoir       : Result := Rect(90 , 60 , 0, 0);
        sBorneInfo      : Result := Rect(0  , 120, 0, 0);
        sCache          : Result := Rect(0  , 0  , 0, 0);
        sTransporteur           : Result := Rect(0  , 60 , 0, 0);
        sCommutateur,
        sCommutateurOff : Result := Rect(120, 120, 0, 0);
        sCommutateurOn  : Result := Rect(90 , 120, 0, 0);
        else Result := Rect(0, 0, 0, 0);
      end;
    end;
    Ciel : Result := Rect(0, 180, 0, 0);
    else Result := Rect(0, 0, 0, 0);
  end;
  Result.Right  := Result.Left + 30;
  Result.Bottom := Result.Top  + 30;
end;

function CaseRectNo(Z : byte) : TRect;
var L, C : byte;
begin
  L := Z div 10;
  C := Z mod 10;
  Result := Rect ((C-1)*30, (L-1)*30, C*30, L*30);
end;

function Point3D(X, Y, Z : integer) : T3DPoint;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function CodeToNoBouton (CodeCase : integer) : integer;
begin
  Result := CodeCase;
  if Result < 128 then Dec(Result, 32) else Dec(Result, 145);
end;

//////////////////////////
/// Classe TCaseActive ///
//////////////////////////

constructor TCaseActive.Create(Owner : TLabyrinthe);
begin
  inherited Create;
  Actions := TScStrings.Create;
  Compteur := 0;
  CaseDesactiver := #0;
  Labyrinthe := Owner;
end;

destructor TCaseActive.Destroy;
begin
  Actions.Free;
  inherited Destroy;
end;

procedure TCaseActive.RemplaceMot(var Str : string; Mot, Chaine : string);
var I : integer;
begin
  while True do
  begin
    I := Recherche(Mot, Str);
    if I = 0 then exit;
    Delete(Str, I, Length(Mot));
    Insert(Chaine, Str, I);
  end;
end;

procedure TCaseActive.RemplaceTousMots(var Str : string; Mot, Chaine : string);
var I : integer;
begin
  while True do
  begin
    I := RechercheTousMots(Mot, Str);
    if I = 0 then exit;
    Delete(Str, I, Length(Mot));
    Insert(Chaine, Str, I);
  end;
end;

procedure TCaseActive.RemplaceSi(var Str : string);
var S, Cond, Vrai, Faux : string;
    I, Si, Alors, Sinon, Fin : integer;
    DebutCond, FinCond,
    DebutVrai, FinVrai,
    DebutFaux, FinFaux : integer;
begin
  S := Str;
  if NbreSubStr('Si'   , S) > 1 then
    raise EInvalidAction.Create('Plusieurs "Si"');
  if NbreSubStr('Alors', S) > 1 then
    raise EInvalidAction.Create('Plusieurs "Alors"');
  if NbreSubStr('Sinon', S) > 1 then
    raise EInvalidAction.Create('Plusieurs "Sinon"');
  if NbreSubStr('FinSi', S) > 1 then
    raise EInvalidAction.Create('Plusieurs "FinSi"');
  Si    := Recherche('Si'   , S);
  Alors := Recherche('Alors', S);
  Sinon := Recherche('Sinon', S);
  Fin   := Recherche('FinSi', S);
  if (Si > 0) and (Alors = 0) then
    raise EInvalidAction.Create('Un Si...Alors...Sinon nécessite'+#10+
                                'un "Si" et un "Alors"');
  if (Alors > 0) and (Si = 0) then
    raise EInvalidAction.Create('Un Si...Alors...Sinon nécessite'+#10+
                                'un "Si" et un "Alors"');
  if (Sinon > 0) and (Si = 0) then
    raise EInvalidAction.Create('Un "Sinon" nécessite un "Si"');
  if (Fin   > 0) and (Si = 0) then
    raise EInvalidAction.Create('Un "FinSi" nécessite un "Si"');
  if Fin   = 0 then Fin := Length(S)+2;
  if Sinon = 0 then Sinon := Fin;
  if (Si > Alors) or (Alors > Sinon) or (Sinon > Fin) then
    raise EInvalidAction.Create('Mauvais ordre dans les mots-clés'+#10+
                                'du Si...Alors...Sinon');
  if Si    = 0 then exit;
  DebutCond := Si    + 3; FinCond := Alors - 1;
  DebutVrai := Alors + 6; FinVrai := Sinon - 1;
  DebutFaux := Sinon + 6; FinFaux := Fin   - 1;
  Cond := Copy(S, DebutCond, FinCond-DebutCond);
  Vrai := Copy(S, DebutVrai, FinVrai-DebutVrai);
  Faux := Copy(S, DebutFaux, FinFaux-DebutFaux);
  I := Pos('[', Vrai) + 1;
  if I = 1 then Vrai := '' else Vrai := Copy(Vrai, I, PosEx(']', Vrai, I)-I);
  I := Pos('[', Faux) + 1;
  if I = 1 then Faux := '' else Faux := Copy(Faux, I, PosEx(']', Faux, I)-I);
  Delete(Str, Si, Fin+5-Si);
  if EvalBool(Cond) then S := Vrai else S := Faux;
  Insert(S, Str, Si);
end;

procedure TCaseActive.RemplaceChaine(var Str : string; Chaine1, Chaine2 : string);
var I : integer;
begin
  while True do
  begin
    I := pos(Chaine1, Str);
    if I = 0 then exit;
    Delete(Str, I, Length(Chaine1));
    Insert(Chaine2, Str, I);
  end;
end;

procedure TCaseActive.RemplaceCase(var Str : string);
var I, X, Y, Z : integer;
    S : string;
    Crochets : boolean;
begin
  if (Str[1] = '[') and (Str[Length(Str)] = ']') then
  begin
    Delete(Str, 1, 1);
    Delete(Str, Length(Str), 1);
    Crochets := True;
  end else Crochets := False;
  while True do
  begin
    I := Recherche('Case', Str);
    if I = 0 then Break;
    S := Copy(Str, I, Length(Str));
    S := GetXWord(S, 1)+' '+GetXWord(S, 2)+' '+
         GetXWord(S, 3)+' '+GetXWord(S, 4);
    X := StrToIntDef(GetXWord(S, 2), -1);
    Y := StrToIntDef(GetXWord(S, 3), -1);
    Z := StrToIntDef(GetXWord(S, 4), -1);
    RemplaceMot(Str, S, Char(Labyrinthe[X, Y, Z]));
  end;
  if Crochets then Str := '['+Str+']';
end;

function TCaseActive.NbreSubStr(SubStr, Str : string) : integer;
var I : integer;
begin
  Result := 0;
  if (SubStr = '') or (Str = '') then exit;
  if SubStr <> ' ' then
  begin
    SubStr := ' '+SubStr+' ';
    Str    := ' '+Str   +' ';
  end;
  I := 1;
  while I <= Length(Str) do
  begin
    if Str[I] = '[' then
    begin
      while (I <= Length(Str)) and (Str[I] <> ']') do inc(I);
      inc(I);
      if I > Length(Str) then exit else Continue;
    end;
    if Str[I] = '{' then
    begin
      while (I <= Length(Str)) and (Str[I] <> '}') do inc(I);
      inc(I);
      if I > Length(Str) then exit else Continue;
    end;
    if Copy(Str, I, Length(SubStr)) = SubStr then inc(Result);
    inc(I);
  end;
end;

function TCaseActive.Recherche(SubStr, Str : string) : integer;
var I : integer;
begin
  Result := 0;
  if (SubStr = '') or (Str = '') then exit;
  if SubStr <> ' ' then
  begin
    SubStr := ' '+SubStr+' ';
    Str    := ' '+Str   +' ';
  end;
  I := 1;
  while I <= Length(Str) do
  begin
    if Str[I] = '[' then
    begin
      while (I <= Length(Str)) and (Str[I] <> ']') do inc(I);
      inc(I);
      if I > Length(Str) then Break else Continue;
    end;
    if Str[I] = '{' then
    begin
      while (I <= Length(Str)) and (Str[I] <> '}') do inc(I);
      inc(I);
      if I > Length(Str) then Break else Continue;
    end;
    if Copy(Str, I, Length(SubStr)) = SubStr then Break;
    inc(I);
  end;
  if I <= Length(Str) then Result := I else Result := 0;
end;

function TCaseActive.RechercheTousMots(SubStr, Str : string) : integer;
var I : integer;
begin
 Result := 0;
 if (SubStr = '') or (Str = '') then exit;
 if SubStr <> ' ' then
 begin
   SubStr := ' '+SubStr+' ';
   Str    := ' '+Str   +' ';
 end;
 I := 1;
 while I <= Length(Str) do
 begin
   if (Str[I] = '[') or (Str[I] = ']') then Str[I] := ' ';
   inc(I);
 end;
 I := 1;
 while I <= Length(Str) do
 begin
   if Copy(Str, I, Length(SubStr)) = SubStr then Break;
   inc(I);
 end;
 if I <= Length(Str) then Result := I else Result := 0;
end;

procedure TCaseActive.RemplaceToutesVariables(var Str : string);
var I : integer;
    X0, Y0, Z0 : integer;
    Str2 : string;
begin
  X0 := Position.X;
  Y0 := Position.Y;
  Z0 := Position.Z;
  RemplaceTousMots(Str, 'X', IntToStr(X0));
  RemplaceTousMots(Str, 'Y', IntToStr(Y0));
  RemplaceTousMots(Str, 'Z', IntToStr(Z0));
  RemplaceTousMots(Str, 'Ici', 'Case '+IntToStr(X0)+' '+IntToStr(Y0)+' '+IntToStr(Z0));
  case Direction of
    Haut : inc(Y0);
    Bas  : dec(Y0);
    Gauche : inc(X0);
    Droite : dec(X0);
  end;
  RemplaceTousMots(Str, 'Devant', 'Case '+IntToStr(X0)+' '+IntToStr(Y0)+' '+IntToStr(Z0));
  case Direction of
    Haut : dec(Y0, 2);
    Bas  : inc(Y0, 2);
    Gauche : dec(X0, 2);
    Droite : inc(X0, 2);
  end;
  RemplaceTousMots(Str, 'Derriere', 'Case '+IntToStr(X0)+' '+IntToStr(Y0)+' '+IntToStr(Z0));
  RemplaceTousMots(Str, 'Direction' , IntToStr(integer(Direction)));
  RemplaceTousMots(Str, 'Reponse'   , IntToStr(Labyrinthe.Reponse));
  RemplaceTousMots(Str, 'Reussite'  , IntToStr(Labyrinthe.Reussite));
  RemplaceTousMots(Str, 'Touche'    , IntToStr(integer(Labyrinthe.Touche)));
  RemplaceTousMots(Str, 'Phase'     , IntToStr(Labyrinthe.Etape)); //4.2.2
  RemplaceTousMots(Str, 'Bouees'    , IntToStr(Bouees));
  RemplaceTousMots(Str, 'Bouee'     , IntToStr(Bouees));   //pour compatibilté anciennes versions
  RemplaceTousMots(Str, 'Planches'  , IntToStr(Planches));
  RemplaceTousMots(Str, 'Planche'   , IntToStr(Planches)); //pour compatibilité anciennes versions
  RemplaceTousMots(Str, 'ClesArgent', IntToStr(ClesArgent));
  RemplaceTousMots(Str, 'ClesOr'    , IntToStr(ClesOr));
  RemplaceTousMots(Str, 'Barque'    , IntToStr(NoBarque));
  RemplaceTousMots(Str, 'Version'   , IntToStr(VersionInterpreteur));
  RemplaceTousMots(Str, 'Couleur'   , IntToStr(CouleurPion));
  if Pos('Compteur', Str) <> 0 then
  begin
    RemplaceTousMots(Str, 'Compteur', IntToStr(Compteur));
    RemplaceTousMots(Str, 'CompteurFin', IntToStr(Labyrinthe.ActionsFin.Compteur));
    if Pos('CompteurBouton_', Str) <> 0 then
      for I := 1 to 45 do
      begin
        Str2 := 'CompteurBouton_'+IntToStr(I);
        RemplaceTousMots(Str, Str2, IntToStr(Labyrinthe.Boutons[I].Compteur));
      end;
    if Pos('CompteurTeleporteur_', Str) <> 0 then
      for I := 1 to 30 do
      begin
        Str2 := 'CompteurTeleporteur_'+IntToStr(I);
        RemplaceTousMots(Str, Str2, IntToStr(Labyrinthe.Teleporteurs[I].Compteur));
      end;
  end;
  if Pos('Variable_', Str) <> 0 then
    for I := 1 to 20 do
    begin
      Str2 := 'Variable_'+IntToStr(I);
      RemplaceTousMots(Str, Str2, IntToStr(Labyrinthe.Variables[I]));
    end;
  if Pos('Compteur', Str) <> 0 then
  begin
    if Pos('CompteurBouton ', Str) <> 0 then
      for I := 1 to 45 do
      begin
        Str2 := '[CompteurBouton '+IntToStr(I)+']';
        RemplaceMot(Str, Str2, IntToStr(Labyrinthe.Boutons[I].Compteur));
      end;
    if Pos('CompteurTeleporteur ', Str) <> 0 then
      for I := 1 to 30 do
      begin
        Str2 := '[CompteurTeleporteur '+IntToStr(I)+']';
        RemplaceMot(Str, Str2, IntToStr(Labyrinthe.Teleporteurs[I].Compteur));
      end;
  end;
  if Pos('Variable ', Str) <> 0 then
    for I := 1 to 20 do
    begin
      Str2 := '[Variable '+IntToStr(I)+']';
      RemplaceMot(Str, Str2, IntToStr(Labyrinthe.Variables[I]));
    end;
  RemplaceChaine(Str, '&Variable ', '&Variable_'); //pour compatibilité anciennes versions
  RemplaceChaine(Str, '&CompteurBouton ', '&CompteurBouton_');  //pour compatibilité anciennes versions
  RemplaceChaine(Str, '&CompteurTeleporteur ', '&CompteurTeleporteur_'); //pour compatibilité anciennes versions
end;

function TCaseActive.GetXWord(Str : string; X : integer) : string;
var I : integer;
begin
  Result := '';
  dec(X);
  while X > 0 do
  begin
    I := Recherche(' ', Str);
    if I = 0 then exit;
    Delete(Str, 1, I);
    dec(X);
  end;
  I := Recherche(' ', Str);
  if I = 0 then I := Length(Str)+1;
  Result := Copy(Str, 1, I-1);
end;

function TCaseActive.StrToIntSpec(Str : string; Negatifs : boolean) : integer;
var Erreur : integer;
begin
  Result := 0;
  if (not Negatifs) and VerifieIntPos(Str) then Result := StrToInt(Str) else
  begin
    Erreur := 1;
    if Negatifs then Val(Str, Result, Erreur);
    if Erreur <> 0 then
    begin
      if Length(Str) <> 3 then raise EInvalidAction.Create('Nombre naturel non valide');
      if (Str[1] <> '[') or (Str[3] <> ']') then EInvalidAction.Create('Nombre naturel non valide');
      Result := Byte(Str[2]);
    end;
  end;
end;

function TCaseActive.EvalBool(StrBool : string) : boolean;
var Conds : TStringList;
    Liens : string;
    I : integer;
begin
  Liens := '';
  if Recherche('Et', StrBool) = 0 then Liens := 'Ou';
  if Recherche('Ou', StrBool) = 0 then Liens := 'Et';
  if Liens = '' then
    raise EInvalidAction.Create('Mélange de "et" et de "ou"');
  Conds := TStringList.Create;
  Conds.Clear;
  while StrBool <> '' do
  begin
    I := Recherche(Liens, StrBool);
    if I = 0 then I := Length(StrBool)+2;
    Conds.Add(Copy(StrBool, 1, I-2));
    Delete(StrBool, 1, I+2);
  end;
  Result := (Liens = 'Et');
  for I := 0 to Conds.Count-1 do if EvalUnBool(Conds[I]) <> Result then
  begin
    Result := not Result;
    exit;
  end;
end;

function TCaseActive.EvalUnBool(Str : string) : boolean;
var Operation : string;
    Params : array [1..2] of string;
    IntParams : array [1..2] of integer;
    IndexParam : integer;
begin
  if NbreSubStr(' ', Str) <> 2 then
    raise EInvalidAction.Create('Expression invalide');
  Result := False;
  Operation := GetXWord(Str, 2);
  if (Operation = '=') or (Operation = '<>') or
     (Operation = '<') or (Operation = '>')  or
     (Operation = 'DivisiblePar') then
  begin
    Params[1] := GetXWord(Str, 1);
    Params[2] := GetXWord(Str, 3);
  end else raise EInvalidAction.Create('Opération inconnue');
  for IndexParam := 1 to 2 do
  begin
    Str := Params[IndexParam];
    if Str = '' then
      raise EInvalidAction.Create('Paramètre vide');
    RemplaceToutesVariables(Str);
    RemplaceCase(Str);
    IntParams[IndexParam] := StrToIntSpec(Str, True);
  end;
  if Operation = '='  then Result := (IntParams[1] =  IntParams[2]) else
  if Operation = '<>' then Result := (IntParams[1] <> IntParams[2]) else
  if Operation = '<'  then Result := (IntParams[1] <  IntParams[2]) else
  if Operation = '>'  then Result := (IntParams[1] >  IntParams[2]) else
  if Operation = 'DivisiblePar' then Result := (IntParams[2] <> 0) and ((IntParams[1] mod IntParams[2]) = 0);
end;

function TCaseActive.CharSuivant(C : Char; S : string) : Char;
var I : integer;
begin
  if Length(S) = 0 then Result := ' '
  else if Length(S) = 1 then Result := S[1]
  else
  begin
    Result := C;
    I := Pos(C, S);
    if I = 0 then exit;
    inc(I); if I > Length(S) then I := 1;
    Result := S[I];
  end;
end;

function TCaseActive.VerifieBonsChar(Str : string) : boolean;
var I : integer;
const BonsChar = ['!'..'Z', '`'..'~', '¡'..'¾', 'Á'..'Ê', 'à'..'â'];
begin
  Result := False;
  if Str = '' then exit;
  for I := 1 to Length(Str) do if not (Str[I] in BonsChar) then exit;
  Result := True;
end;

function TCaseActive.VerifieBonMessage(var Str : string) : boolean;
var I : integer;
begin
  Result := False;
  if Str = '' then exit;
  for I := 1 to Length(Str) do if ((Str[I] <> ' ') and (Str[I] <> '\')) then
  begin
    Result := True;
    Break;
  end;
  if Result then for I := 1 to Length(Str) do
    if Str[I] = '\' then Str[I] := #10;
end;

procedure TCaseActive.SetActions(Acts : TScStrings);
begin
  Actions.Assign(Acts);
end;

function TCaseActive.Execute(var Ici : T3DPoint; var ABouge : boolean) : boolean;
var I, J : integer;
    Str, Command : string;
    AFaitMessage : boolean;
label Fin;
begin
  Result := False;
  AFaitMessage := False;
  EstAlleARien := False;
  ABouge := False;
  //Infos := InfosJeu;
  Position := Ici;
  StrIci := concat('Case ', IntToStr(Ici.X), ' ', IntToStr(Ici.Y),' ', IntToStr(Ici.Z));
  inc(Compteur);
  I := 0;
  while I < Actions.Count do
  begin
    Str := Actions[I];
    inc(I);
    if (Str = '') or (Str[1] = '#') or
       (GetXToken(Str, ' ', 1) = 'Remarque') then Continue;
    try
      RemplaceSi(Str);
    except
      on E : EInvalidAction do
      begin
        if TestActions then ShowMes('Erreur dans les actions',
           'Erreur dans le Si...Alors...Sinon à la ligne '+IntToStr(I)+' :'+#10+#10+
           E.Message, MB_OK or MB_ICONERROR);
        Continue;
      end;
    end;
    if Str = '' then Continue;
    if GetXToken(Str, ' ', 1) = 'Unique' then
    begin
      if Compteur <> 1 then Continue;
      Delete(Str, 1, 7);
    end;
    Command := GetXToken(Str, ' ', 1);
    Delete(Str, 1, Length(Command)+1);
    if (Command = 'Stop') then goto Fin;
    RemplaceToutesVariables(Str);
    try
      if Command = 'Remplacer'   then ExecuteRemplacer(Str) else
      if Command = 'Desactiver'  then
        begin
          if Str = '' then
            ExecuteRemplacer('Ici '+CaseDesactiver)
          else
            ExecuteRemplacer('Ici '+Str);
          Labyrinthe.Reussite := 1;
        end else
      if Command = 'Convertir'   then ExecuteConvertir(Str) else
      if Command = 'Deplacer'    then ExecuteDeplacer(Str) else
      if Command = 'Incrementer' then ExecuteIncr(Str, 1) else
      if Command = 'Decrementer' then ExecuteIncr(Str, -1) else
      if Command = 'Multiplier'  then ExecuteMult(Str) else
      if Command = 'Message'     then AFaitMessage := ExecuteMessage(Str) else
      if Command = 'Echec'       then AFaitMessage := ExecuteEchec(Str) else
      if Command = 'Impasse'     then
      begin
        if TouchePressee then ExecuteImpasse(Str);
      end else
      if Command = 'Choix'       then ExecuteChoix(Str) else
      if Command = 'Son'         then ExecuteSon(Str) else
      if Command = 'AllerA'      then
      begin
        if ExecuteAllerA(Str) then ABouge := True;
      end else
      if Command = 'LaisserPasser' then
      begin
        Labyrinthe.Reussite := 1;
        if ExecuteAllerA('Ici') then ABouge := True;
      end else
      if Command = 'AutoriserPlanche' then Labyrinthe.AutorisePlanche := True else
      if Command = 'Arreter'          then Labyrinthe.Poursuivre(false) else
      if Command = 'Poursuivre'       then Labyrinthe.Poursuivre(true) else
      if Command = 'Continuer'        then Labyrinthe.Continuer := True else //4.2.2
      if Command = 'Description'      then ExecuteDescription else
      if Command = 'Gagner'           then AFaitMessage := ExecuteGagne(Str) else
      if Command = 'Indice' then
      begin
        if Indices then ExecuteIndice(Str);
      end else
      if Command = 'Saute' then
      begin
        RemplaceCase(Str);
        J := Actions.IndexOf('#'+GetXToken(Str, ' ', 1));
        if J <> -1 then I := J+1;
      end else
      if TestActions then
        ShowMes('Erreur dans les actions', 'Commande inconnue à la ligne '+IntToStr(I),
                MB_OK or MB_ICONERROR);
    except
      on E : EInvalidAction do
      begin
        if TestActions then ShowMes('Erreur dans les actions',
           'Erreur de paramètres de commande à la ligne '+IntToStr(I)+' :'+#10+#10+
           E.Message, MB_OK or MB_ICONERROR);
        Continue;
      end;
    end;
  end; // while
Fin :
  if ABouge then
    Labyrinthe.AutorisePlanche := False;
  if (Ici.X <> Position.X) or (Ici.Y <> Position.Y) or (Ici.Z <> Position.Z) then
  begin
    Sleep(Temporisation);
    TouchePressee := False;
    Direction := SansDirection;
    Labyrinthe.Attendu := True;
    Ici := Position;
    if ClassType = TCaseTelepor then Result := False;
  end else if ClassType = TCaseTelepor then Result := (not EstAlleARien);
  //InfosJeu := Infos;
  if ClassType = TCaseFin     then Result := (not AFaitMessage);
end;

procedure TCaseActive.AllerA(Str : string; var Ici : T3DPoint);
begin
  Position := Ici;
  ExecuteAllerA(Str);
  Ici := Position;
end;

procedure TCaseActive.ExecuteDeplacer(Str : string);
var Param : string;
    C, CR : Char;
    X, Y, Z : integer;
begin
  Param := GetXToken(Str, ' ', 1);
  if Length(Param) = 1 then C := Param[1] else
    raise EInvalidAction.Create('Code de case unique attendu');
  if not VerifieBonsChar(Param) then
    raise EInvalidAction.Create('Code de case de remplacement incorrect');
  if (Byte(C) in Barque) then CR := '1' else CR := '0';
  Param := GetXToken(Str, ' ', 2);
  if Param <> 'Case' then
    raise EInvalidAction.Create('Paramètre incorrect');
  X := StrToIntDef(GetXToken(Str, ' ', 3), -1);
  Y := StrToIntDef(GetXToken(Str, ' ', 4), -1);
  Z := StrToIntDef(GetXToken(Str, ' ', 5), -1);
  if (Labyrinthe.Exterieur(X, Y, Z, true)) then
    raise EInvalidAction.Create('Déplacement impossible à l''extérieur du labyrinthe');
  ExecuteConvertir(C+' '+CR);
  Labyrinthe[X, Y, Z] := Byte(C);
end;

procedure TCaseActive.ExecuteRemplacer(Str : string);
var SousCommand, SSC : string;
    X, Y, Z : integer;
    C : Char;
begin
  RemplaceMot(Str, 'Ici', StrIci);
  SousCommand := GetXToken(Str, ' ', 1);
  SSC := GetXToken(Str, ' ', 2);
  if SousCommand = 'Case' then
  begin
    X := StrToIntDef(GetXToken(Str, ' ', 2), -1);
    Y := StrToIntDef(GetXToken(Str, ' ', 3), -1);
    Z := StrToIntDef(GetXToken(Str, ' ', 4), -1);
    Str := GetXToken(Str, ' ', 5);
    if Labyrinthe.Exterieur (X, Y, Z, false) then
         raise EInvalidAction.Create('Remplacement impossible à l''extérieur du labyrinthe');
    if (Str <> '') and VerifieBonsChar(Str) then
    begin
      C := CharSuivant(Char(Labyrinthe[X, Y, Z]), Str);
      Labyrinthe[X, Y, Z] := Byte(C);
    end else
      raise EInvalidAction.Create('Code(s) de case(s) de remplacement incorrect(s)');
  end else
  if SousCommand = '&Compteur' then
  begin
    X := StrToIntDef(SSC, -1);
    if X >= 0 then Compteur := X else
      raise EInvalidAction.Create('Valeur pour compteur incorrecte');
  end else
  if Pos('&CompteurBouton', SousCommand) = 1 then
  begin
    X := StrToIntDef(GetLastToken(SousCommand, '_'), -1);
    Y := StrToIntDef(SSC, -1);
    if (X < 1) or (X > 45) then
      raise EInvalidAction.Create('Numéro de bouton incorrect');
    if Y < 0 then
      raise EInvalidAction.Create('Valeur pour compteur incorrecte');
    Labyrinthe.Boutons[X].Compteur := Y;
  end else
  if Pos('&CompteurTeleporteur', SousCommand) = 1  then
  begin
    X := StrToIntDef(GetLastToken(SousCommand, '_'), -1);
    Y := StrToIntDef(SSC, -1);
    if (X < 1) or (X > 30) then
      raise EInvalidAction.Create('Numéro de téléporteur incorrect');
    if Y < 0 then
      raise EInvalidAction.Create('Valeur pour compteur incorrecte');
    Labyrinthe.Teleporteurs[X].Compteur := Y;
  end else
  if SousCommand = '&CompteurFin' then
  begin
    X := StrToIntDef(SSC, -1);
    if X >= 0 then Labyrinthe.ActionsFin.Compteur := X else
      raise EInvalidAction.Create('Valeur pour compteur incorrecte');
  end else
  if (SousCommand = '&Bouee') or (SousCommand = '&Bouees') then
  begin
    X := StrToIntDef(SSC, -1);
    if X >= 0 then Bouees := X else
      raise EInvalidAction.Create('Valeur pour le nombre de bouées incorrecte');
  end else
  if (SousCommand = '&Planche') or (SousCommand = '&Planches') then
  begin
    X := StrToIntDef(SSC, -1);
    if X >= 0 then Planches := X else
      raise EInvalidAction.Create('Valeur pour le nombre de planches incorrecte');
  end else
  if SousCommand = '&ClesArgent' then
  begin
    X := StrToIntDef(SSC, -1);
    if X >= 0 then ClesArgent := X else
      raise EInvalidAction.Create('Valeur pour le nombre de clés d''argent incorrecte');
  end else
  if SousCommand = '&ClesOr' then
  begin
    X := StrToIntDef(SSC, -1);
    if X >= 0 then ClesOr := X else
      raise EInvalidAction.Create('Valeur pour le nombre de clés d''or incorrecte');
  end else
  if SousCommand = '&Couleur' then
    CouleurPion := EvalCouleur(SSC)
  else
  if SousCommand = '&Temporisation' then
    Temporisation := StrToIntDef(SSC, 500)
  else
  if SousCommand = '&X' then
  begin
    X := StrToIntDef(SSC, -1);
    if (X >= -1) and (X <= Labyrinthe.Dimensions.Colonnes*7)
      then Position.X := X
      else raise EInvalidAction.Create('Valeur pour la coordonnée X incorrecte');
  end else
  if SousCommand = '&Y' then
  begin
    Y := StrToIntDef(SSC, -1);
    if (Y >= -1) and (Y <= Labyrinthe.Dimensions.Lignes*7)
      then Position.Y := Y
      else raise EInvalidAction.Create('Valeur pour la coordonnée Y incorrecte');
  end else
  if SousCommand = '&Z' then
  begin
    Z := StrToIntDef(SSC, -1);
    if (Z > 0) and (Z <= Labyrinthe.Dimensions.Etages)
      then Position.Z := Z
      else raise EInvalidAction.Create('Valeur pour la coordonnée Z incorrecte');
  end else
  if SousCommand = 'Bord' then
  begin
    for Z := 1 to maxbord do
    begin
      C := CharSuivant(Char(Bord[Z]), GetXToken(Str, ' ', 1+Z));
      if (C = 'B') or (C = 'â') or ((C >= '0') and (C <= '3')) then Bord[Z] := Byte(C) else Bord[Z] := Bord[Z-1];
    end;
  end else
  if SousCommand = '&Reussite' then
  begin
    X := StrToIntDef(SSC, -1);
    if (X = 0) or (X = 1) then Labyrinthe.Reussite := X else
      raise EInvalidAction.Create('Valeur de "Reussite" invalide');
  end else
  if SousCommand = '&Direction' then
  begin
    X := StrToIntDef(SSC, -1);
    if (X >= 0) and (X <= 4) then Direction := TFleche(X) else
      raise EInvalidAction.Create('Valeur de Direction invalide');
  end else
  if Pos('&Variable', SousCommand) = 1 then
  begin
    X := StrToIntDef(GetLastToken(SousCommand, '_'), 0);
    if SSC = 'Aleatoire' then
    begin
      Z := StrToIntDef(GetXToken(Str, ' ', 3), -1);
      if Z < 0 then raise EInvalidAction.Create('Valeur incorrecte pour "Aleatoire"');
      Y := random(Z);
    end
    else
      Y := StrToIntDef(SSC, 0);
    if (X < 1) or (X > 20) then
      raise EInvalidAction.Create('Numéro de variable incorrect');
    Labyrinthe.Variables[X] := Y;
  end else
    raise EInvalidAction.Create('Sous-commande "'+SousCommand+'" de "Remplacer" inconnue');
end;  //ExecuteRemplacer

procedure TCaseActive.ExecuteConvertir(Str : string);
var X, Y, Z : integer;
    CodeAvant, CodeApres : integer;
    CharAvant, CharApres : string;
begin
  CharAvant := GetXToken(Str, ' ', 1);
  CharApres := GetXToken(Str, ' ', 2);
  if (Length(CharAvant) = 1) and VerifieBonsChar(CharAvant)
    then CodeAvant := Byte(CharAvant[1]) else
    raise EInvalidAction.Create('Code de case à remplacer incorrect');
  if (Length(CharApres) = 1) and VerifieBonsChar(CharApres)
    then CodeApres := Byte(CharApres[1]) else
    raise EInvalidAction.Create('Code de case de remplacement incorrect');
  for X := 0 to Labyrinthe.Dimensions.Colonnes*7 - 1 do
    for Y := 0 to Labyrinthe.Dimensions.Lignes*7 - 1 do
      for Z := 1 to Labyrinthe.Dimensions.Etages do
        if Labyrinthe[X, Y, Z] = CodeAvant then
          Labyrinthe[X, Y, Z] := CodeApres;
end;

procedure TCaseActive.ExecuteMult(Str : string);
var Variable : string;
    I : integer;
begin
  Variable := GetXToken(Str, ' ', 1);
  if Pos ('&Variable', Variable) = 1 then
  begin
    I := StrToIntDef(GetLastToken(Variable, '_'), 0);
    if (I < 1) or (I > 20) then
      raise EInvalidAction.Create('Numéro de variable incorrect');
    Labyrinthe.Variables[I] := Labyrinthe.Variables[I]*StrToIntDef(GetXToken(Str, ' ', 2), 1);
  end else
  raise EInvalidAction.Create('Sous-commande de "Multiplier" incorrecte');
end;

procedure TCaseActive.ExecuteIncr(Str : string; Incr : integer);
var Variable : string;
    I, J, K : integer;
procedure IncrCompteur(var C : integer; Incr : integer);
    begin
      inc(C, Incr);
      if C < 0 then
      begin
        C := 0;
        raise EInvalidAction.Create('Valeur de compteur négative')
      end;
    end;
begin
  Variable := GetXToken(Str, ' ', 1);
  Incr := Incr * StrToIntDef(GetXToken(Str, ' ', 2), 1);
  if (Variable = '&Bouees') then IncrCompteur(Bouees, Incr)
  else if (Variable = '&Planches') then IncrCompteur(Planches, Incr)
  else if (Variable = '&ClesArgent') then IncrCompteur(ClesArgent, Incr)
  else if (Variable = '&ClesOr') then IncrCompteur(ClesOr, Incr)
  else if (Variable = '&Compteur') then IncrCompteur(Compteur, Incr)
  else
  if Pos('&CompteurBouton', Variable) = 1 then
  begin
    I := StrToIntDef(GetLastToken(Variable, '_'), 0);
    if (I < 1) or (I > 45) then
      raise EInvalidAction.Create('Numéro de bouton incorrect');
    IncrCompteur(Labyrinthe.Boutons[I].Compteur, Incr);
  end else
  if Pos('&CompteurTeleporteur', Variable) = 1 then
  begin
    I := StrToIntDef(GetLastToken(Variable, '_'), 0);
    if (I < 1) or (I > 30) then
      raise EInvalidAction.Create('Numéro de téléporteur incorrect');
    IncrCompteur(Labyrinthe.Teleporteurs[I].Compteur, Incr);
  end else
  if (Variable = '&CompteurFin') then
    IncrCompteur(Labyrinthe.ActionsFin.Compteur, Incr)
  else if (Variable = '&X') or (Variable = '&Y') or (Variable = '&Z') then
  begin
    I := Position.X; J := Position.Y; K := Position.Z;
    if (Variable = '&X') then Inc(I, Incr)
    else if (Variable = '&Y') then Inc(J, Incr)
    else if (Variable = '&Z') then Inc(K, Incr);
    if Labyrinthe.Exterieur(I,J,K,true)
    then raise EInvalidAction.Create('Impossible d''envoyer le pion hors du labyrinthe')
    else begin Position.X := I; Position.Y := J; Position.Z := K; end;
  end else
  if Pos ('&Variable', Variable) = 1 then
  begin
    I := StrToIntDef(GetLastToken(Variable, '_'), 0);
    if (I < 1) or (I > 20) then
      raise EInvalidAction.Create('Numéro de variable incorrect');
    inc(Labyrinthe.Variables[I], Incr);
  end else
  raise EInvalidAction.Create('Sous-commande de "Incrementer" ou "Decrementer" incorrecte');
end;

function TCaseActive.ExecuteAllerA(Str : string) : boolean;
var SousCommand : string;
    I, J, K, X, Y, Z, SorteCase : integer;
    ListeCases : TStringList;
    Cols, Rows, Etas : integer;
begin
  Result := True;
  //if (Str <> 'Rien') and (Str <> 'Ici') then Direction := SansDirection;
  RemplaceMot(Str, 'Ici', StrIci);
  X := Position.X;
  Y := Position.Y;
  Z := Position.Z;
  Cols := Labyrinthe.Dimensions.Colonnes;
  Rows := Labyrinthe.Dimensions.Lignes;
  Etas := Labyrinthe.Dimensions.Etages;
  SousCommand := GetXToken(Str, ' ', 1);
  Delete(Str, 1, Length(SousCommand)+1);
  if SousCommand = 'Suivant' then
  begin
    Str := GetXToken(Str, ' ', 1);
    if (Length(Str) <> 1) or (not VerifieBonsChar(Str)) then
      raise EInvalidAction.Create('Code de case incorrect');
    SorteCase := Byte(Str[1]);
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
    until (Labyrinthe[X, Y, Z] = SorteCase) or
          ((X = Position.X) and (Y = Position.Y) and (Z = Position.Z));
  end else if SousCommand = 'Precedent' then
  begin
    Str := GetXToken(Str, ' ', 1);
    if (Length(Str) <> 1) or (not VerifieBonsChar(Str)) then
      raise EInvalidAction.Create('Code de case incorrect');
    SorteCase := Byte(Str[1]);
    repeat
      dec(X);
      if X = -1 then
      begin
        X := Cols*7-1;
        dec(Y);
        if Y = -1 then
        begin
          Y := Rows*7-1;
          dec(Z);
          if Z = 0 then Z := Etas;
        end;
      end;
    until (Labyrinthe[X, Y, Z] = SorteCase) or
          ((X = Position.X) and (Y = Position.Y) and (Z = Position.Z));
  end else if SousCommand = 'Aleatoire' then
  begin
    Str := GetXToken(Str, ' ', 1);
    if (Length(Str) <> 1) or (not VerifieBonsChar(Str)) then
      raise EInvalidAction.Create('Code de case incorrect');
    SorteCase := Byte(Str[1]);
    ListeCases := TStringList.Create;
    for I := 0 to Cols*7-1 do
      for J := 0 to Rows*7-1 do
        for K := 1 to Etas do
          if ((I <> X) or (J <> Y) or (K <> Z)) and (Labyrinthe[I, J, K] = SorteCase) then
            ListeCases.Add(IntToStr(I)+';'+IntToStr(J)+';'+IntToStr(K));
    if ListeCases.Count = 0 then
    begin
      ListeCases.Free;
      exit;
    end;
    Str := ListeCases[Random(ListeCases.Count)];
    ListeCases.Free;
    X := StrToInt(GetXToken(Str, ';', 1));
    Y := StrToInt(GetXToken(Str, ';', 2));
    Z := StrToInt(GetXToken(Str, ';', 3));
  end else if SousCommand = 'Case' then
  begin
    X := StrToIntDef(GetXToken(Str, ' ', 1), -1);
    Y := StrToIntDef(GetXToken(Str, ' ', 2), -1);
    Z := StrToIntDef(GetXToken(Str, ' ', 3), -1);
    if Labyrinthe.Exterieur (X, Y, Z, true) then
         raise EInvalidAction.Create('Impossible d''envoyer le pion hors du labyrinthe');
  end else if SousCommand = 'Rien' then
  begin
    EstAlleARien := True;
    Result := False;
    exit;
  end else raise EInvalidAction.Create('Sous-commande de "AllerA" incorrecte');
  Position.X := X;
  Position.Y := Y;
  Position.Z := Z;
end;

function TCaseActive.ExecuteInformation(TitreInfo, Str : string; mb : integer) : boolean;
var I, J : integer;
begin
  if Str = 'Rien' then
  begin
    Result := True;
    exit;
  end;
  I := Pos('{', Str) + 1;
  if I = 1 then
    raise EInvalidAction.Create(TitreInfo+' incorrect');
  J := PosEx('}', Str, I) - I;
  Str := Copy(Str, I, J);
  if Pos('$', Str) <> 0 then
  begin
    if Pos('$Compteur', Str) <> 0 then
    begin
     if Pos('$CompteurBouton', Str) <> 0 then
       for I := 45 downto 1 do RemplaceVariable(Str, '$CompteurBouton_'+
         IntToStr(I), IntToStr(Labyrinthe.Boutons[I].Compteur));
     if Pos('$CompteurTeleporteur', Str) <> 0 then
       for I := 30 downto 1 do RemplaceVariable(Str, '$CompteurTeleporteur_'+
         IntToStr(I), IntToStr(Labyrinthe.Teleporteurs[I].Compteur));
     RemplaceVariable(Str, '$CompteurFin', IntToStr(Labyrinthe.ActionsFin.Compteur));
     RemplaceVariable(Str, '$Compteur', IntToStr(Compteur));
    end; //$Compteur...
   if Pos('$Variable', Str) <> 0 then
     for I := 20 downto 1 do RemplaceVariable(Str, '$Variable_'+IntToStr(I),
                                              IntToStr(Labyrinthe.Variables[I]));
    RemplaceVariable(Str, '$Bouees'    , IntToStr(Bouees    ));
    RemplaceVariable(Str, '$Planches'  , IntToStr(Planches  ));
    RemplaceVariable(Str, '$ClesArgent', IntToStr(ClesArgent));
    RemplaceVariable(Str, '$ClesOr'    , IntToStr(ClesOr    ));
  end; //$...
  if VerifieBonMessage(Str) then
  begin
    case ShowMes(TitreInfo, Str, mb) of
      ID_NO         : Labyrinthe.Reponse := 0;
      ID_YES, ID_OK : Labyrinthe.Reponse := 1;
      ID_CANCEL     : Labyrinthe.Reponse := 2;
    end;
    Result := True;
  end else raise EInvalidAction.Create(TitreInfo+' incorrect');
end;

procedure TCaseActive.ExecuteChoix(Str : string);
var Flags : Word;
    Params : TScStrings;
begin
  Params := TScStrings.CreateFromString(Str, ' ', '{}');
  if Params.Count <> 2 then raise
    EInvalidAction.Create('Paramètres incorrects');
  Str := Params[0];
  if Str = 'Oui-Non' then Flags := MB_YESNO else
  if Str = 'Oui-Non-Annuler' then Flags := MB_YESNOCANCEL else
  if Str = 'OK-Annuler' then Flags := MB_OKCANCEL else
    raise EInvalidAction.Create('Boutons du choix incorrects');
  Flags := Flags or MB_IconQuestion;
  ExecuteInformation('Choix', Params[1], Flags);
end;

procedure TCaseActive.ExecuteSon(Str : string);
var I, J : integer;
begin
  I := Pos('{', Str) + 1;
  if I = 1 then
    EInvalidAction.Create('Son incorrect');
  J := PosEx('}', Str, I) - I;
  Str := Copy(Str, I, J);
  if not ExecuteSound(Str, stSysSound, True) then
  if not ExecuteSound(Dir+'Sons\'+Str, stFileName, True) then
    raise EInvalidAction.Create('Son inexistant');
end;

function TCaseActive.ExecuteMessage(Str : string) : boolean;
begin
  Result := ExecuteInformation('Message', Str, MB_ICONINFORMATION or MB_OK);
end;

procedure TCaseActive.ExecuteDescription;
var I : integer;
    Str : string;
begin
  if Labyrinthe.Description.Count = 0 then exit;
  I := 0;
  while I < Labyrinthe.Description.Count do
  begin
    Str := Str+#13+#10+Labyrinthe.Description[I];
    inc(I);
  end;
  Delete(Str, 1, 2);
  ShowMes('Description', Str, MB_OK or MB_ICONINFORMATION);
end;

procedure TCaseActive.ExecuteIndice(Str : string);
begin
  ExecuteInformation('Indice', Str, MB_ICONEXCLAMATION or MB_OK);
end;

function TCaseActive.ExecuteEchec(Str : string) : boolean;
begin
  Result := ExecuteInformation('Échec', Str, MB_ICONERROR or MB_OK);
end;

procedure TCaseActive.ExecuteImpasse(Str : string);
begin
  ExecuteInformation('Impasse', Str, MB_ICONERROR or MB_OK);
end;

function TCaseActive.ExecuteGagne(Str : string) : boolean;
begin
  AGagne := True;
  Result := false;
  If Str <> '' then Result := ExecuteInformation('Gagné !', Str, MB_ICONINFORMATION or MB_OK);
end;

///////////////////////////////////////////
/// Classes descendantes de TCaseActive ///
///////////////////////////////////////////

constructor TCaseBouton.Create(Owner : TLabyrinthe);
begin
  inherited Create(Owner);
  Image := TBitmap.Create;
  Style := sPoussoir;
  Nom := EmptyStr; //chargera l'image d'un bouton poussoir
end;

destructor TCaseBouton.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;

constructor TCaseTelepor.Create(Owner : TLabyrinthe);
begin
  inherited Create(Owner);
  Image := TBitmap.Create;
  CaseDesactiver := '`';
  Style := sTransporteur;
  Nom := EmptyStr; //chargera l'image d'un téléporteur ordinaire
end;

destructor TCaseTelepor.Destroy;
begin
  Image.Free;
  inherited;
end;

constructor TCaseFin.Create(Owner : TLabyrinthe);
begin
  inherited;
  CaseDesactiver := '0';
end;

constructor TActionsZone.Create(Owner : TLabyrinthe);
begin
  inherited;
  CaseDesactiver := #0;
end;

procedure TCaseBouton.SetStyle(NewStyle : TStyleBouton);
begin
  FStyle := NewStyle;
  if FStyle = sPoussoir then CaseDesactiver := '@'
                        else CaseDesactiver := '0';
  //Nom := Nom;
end;

procedure TCaseBouton.SetNom(New : string);
var Z : byte;
begin
  FNom := New;
  if (FNom <> EmptyStr) and FileExists(Dir+'Cases\'+FNom+'.bmp') then
    Image.LoadFromFile(Dir+'Cases\'+FNom+'.bmp')
  else with Image do
  begin
    Height := 30;
    Width  := 30;
    Z := StrToIntDef(FNom, 34);
    Canvas.CopyRect(Rect(0, 0, 30, 30), Cases.Canvas, CaseRectNo(Z));
  end;
end;

procedure TCaseTelepor.SetStyle(NewStyle : TStyleBouton);
begin
  FStyle := NewStyle;
  if FStyle = sTransporteur then CaseDesactiver := '`'
                    else CaseDesactiver := '0';
  //Nom := Nom;
end;

procedure TCaseTelepor.SetNom(NewNom : string);
var Z : byte;
begin
  FNom := NewNom;
  if (FNom <> EmptyStr) and FileExists(Dir+'Cases\'+FNom+'.bmp') then
    Image.LoadFromFile(Dir+'Cases\'+FNom+'.bmp')
  else with Image do
  begin
    Height := 30;
    Width  := 30;
    Z := StrToIntDef(FNom, 31);
    Canvas.CopyRect(Rect(0, 0, 30, 30), Cases.Canvas, CaseRectNo(Z));
  end;
end;

//////////////////////////
/// Classe TLabyrinthe ///
//////////////////////////

constructor TLabyrinthe.CreateOpen(FileName : string);
var SL, EnTete : TScStrings;
    I, J, X, Y, Z : integer;
    Tmp : LongInt;
    iTmp : integer;
    bTmp : boolean;
    Str : string;
    UnPoint : T3DPoint;
    ABouge : boolean;
begin
  inherited Create;

  {$REGION 'Initialisation globale'}
  SL := TScStrings.Create;
  EnTete := TScStrings.Create;
  SLEnteteBoutons := TScStrings.Create;
  SLEnteteTeleporteurs := TScStrings.Create;
  Init;
  FFileName := FileName;
  FConstruction := (Application.Tag = 2);
  Reponse := 0;
  Etape := -1;
  AGagne := False;
  {$ENDREGION}

  {$REGION 'Initialisation de FLabyrinthe'}
  SL.LoadFromFile(FileName);
  I := SL.IndexOf('[Labyrinthe]');
  FLabyrinthe := TScStrings.Create;
  FLabyrinthe.CopyFrom(SL, I, SL.Count);
  EnTete.CopyFrom(SL, 0, I);
  EnTete.Add('[LaFinDeLEntete]');
  {$ENDREGION}

  {$REGION 'Vérification sur l''édition de sauvegardes'}
  if FConstruction and (EnTete.IndexOf('[Sauvegarde]') <> -1) then
  begin
    EnTete.Free;
    SL.Free;
    raise EIsSauvegarde.Create('Ce fichier est une sauvegarde !');
  end;
  {$ENDREGION}

  {$REGION 'Initialisation de Dimensions'}
  I := EnTete.IndexOf('[Dimensions]');
  if I <> -1 then
  begin
    J := EnTete.FindAtPos('[', 1, I+1);
    SL.CopyFrom(EnTete, I+1, J-I-1);
    FixeInt(SL, 'Colonnes', FDimensions.Colonnes);
    FixeInt(SL, 'Lignes'  , FDimensions.Lignes);
    FixeInt(SL, 'Etages'  , FDimensions.Etages);
  end;
  {$ENDREGION}

  {$REGION 'Initialisation de EditInfos'}
  I := EnTete.IndexOf('[EditInfos]');
  if I <> -1 then
  begin
    J := EnTete.FindAtPos('[', 1, I+1);
    SL.CopyFrom(EnTete, I+1, J-I-1);
    FixeBool(SL, 'PeutEditer', FFileInfos.PeutEditer);
  end;
  {$ENDREGION}

  {$REGION 'Initialisation de Options'}
  I := EnTete.IndexOf('[Options]');
  FNomLab := ExtractFileName(FileName);
  FNomLab := Copy(FNomLab, 1, Length(FNomLab)-Length(ExtractFileExt(FNomLab)));
  FNomCases := 'Cases.bmp';
  //Indices := False;
  if I <> -1 then
  begin
    J := EnTete.FindAtPos('[', 1, I+1);
    SL.CopyFrom(EnTete, I+1, J-I-1);
    FixeStr(SL, 'NomLabyrinthe', FNomLab);
    FixeStr(SL, 'Cases', FNomCases);
    FixeBool(SL, 'Indices', Indices);
  end;
  if FileExists(Dir+'Cases\'+FNomCases) then
    Cases.LoadFromFile(Dir+'Cases\'+FNomCases);
  {$ENDREGION}

  {$REGION 'Initialisation de Description'}
  I := EnTete.IndexOf('[Description]');
  FDescription := TStringList.Create;
  FDescription.Clear;
  if I <> -1 then
  begin
    J := EnTete.FindAtPos('[', 1, I+1);
    SL.CopyFrom(EnTete, I+1, J-I-1);
    FDescription.Assign(SL);
  end;
  {$ENDREGION}

  {$REGION 'Initialisation des actions'}
  LoadActions(EnTete);
  {$ENDREGION}

  {$REGION 'Initialisation de Sauvegarde et de Position'}
  I := EnTete.IndexOf('[Sauvegarde]');
  if I <> -1 then
  begin
    FFileInfos.Sauvegarde := True;
    J := EnTete.FindAtPos('[', 1, I+1);
    SL.CopyFrom(EnTete, I+1, J-I-1);
    FixeStr(SL, 'NomLabyrinthe', FNomLab);
    FixeInt(SL, 'PositionX', X);
    FixeInt(SL, 'PositionY', Y);
    FixeInt(SL, 'Etage', Z);
    bTmp := False;
    FixeBool(SL, 'Bouee', bTmp); //pour compatibilité anciennes versions
    if bTmp then Bouees := 1
            else FixeInt(SL, 'Bouees', Bouees);
    bTmp := False;
    FixeBool(SL, 'Planche', bTmp); //pour compatiblité anciennes versions
    if bTmp then Planches := 1
            else FixeInt(SL, 'Planches', Planches);
    FixeInt(SL, 'PetitesCles', ClesArgent);
    FixeInt(SL, 'GrandesCles', ClesOr);
    FixeInt(SL, 'Barque', NoBarque);
    iTmp := clBlue;
    FixeInt(SL, 'Couleur', iTmp);
    CouleurPion := iTmp;
    iTmp := 500;
    FixeInt(SL, 'Temporisation', iTmp);
    Temporisation := iTmp;
    FixeBool(SL, 'Indices', Indices);
    Str := '';
    FixeStr(SL, 'CompteursBoutons', Str);
    for I := 1 to 45 do FBoutons[I].Compteur :=
      StrToIntDef(GetXToken(Str, ' ', I), 0);
    Str := '';
    FixeStr(SL, 'CompteursTeleporteurs', Str);
    for I := 1 to 30 do FTeleporteurs[I].Compteur :=
      StrToIntDef(GetXToken(Str, ' ', I), 0);
    FixeInt(SL, 'CompteurFin', ActionsFin.Compteur);
    Str := '';
    FixeStr(SL, 'Variables', Str);
    for I := 1 to 20 do Variables[I] :=
      StrToIntDef(GetXToken(Str, ' ', I), 0);
    Str := '';
    FixeStr(SL, 'CompteursZones', Str);
    for I := 0 to FActionsZone.Count-1 do
    begin
      Tmp := StrToInt(FActionsZone.ValueFromIndex[I]);
      TActionsZone(Tmp).Compteur := StrToIntDef(GetXToken(Str, ' ', I+1), 0);
    end;
    Str := EmptyStr;
    FixeStr(SL, 'Bord', Str);
    iTmp := length(Str);
    Bord[0] := Vide;
    for I := 1 to maxbord do
      if iTmp >= 2*I-1 then Bord[I] := Byte(Str[2*I-1]) else Bord[I] := Bord[I-1];
    FPosition.LaCase.X := Div7(X);
    FPosition.LaCase.Y := Div7(Y);
    FPosition.LaCase.Z := Z;
    FPosition.LaPosition.X := Mod7(X);
    FPosition.LaPosition.Y := Mod7(Y);
  end else
  begin //pas de sauvegarde
    for Z := 1 to FDimensions.Etages do
      for Y := 0 to FDimensions.Lignes*7 - 1 do
        for X := 0 to FDimensions.Colonnes*7 - 1 do
          if Labyrinthe[X, Y, Z] = Depart then
          begin
            if not FConstruction then Labyrinthe[X, Y, Z] := Herbe;
            FPosition.LaCase.X := X div 7;
            FPosition.LaCase.Y := Y div 7;
            FPosition.LaCase.Z := Z;
            FPosition.LaPosition.X := X mod 7;
            FPosition.LaPosition.Y := Y mod 7;
          end;
    {Bouees     := 0;
    Planches   := 0;
    ClesArgent := 0;
    ClesOr     := 0;
    Indices := False;}
    if not FConstruction then
    begin
      if not Indices then
      begin
        for I := 1 to 45 do
        begin
          J := 0;
          while J < Boutons[I].LesActions.Count do
          begin
            if Pos('Indice', Boutons[I].LesActions[J]) <> 0 then
            begin
              Indices := True;
              Break;
            end;
            inc(J);
          end;
          if Indices then Break;
        end;
        if not Indices then for I := 1 to 30 do
        begin
          J := 0;
          while J < Teleporteurs[I].LesActions.Count do
          begin
            if Pos('Indice', Teleporteurs[I].LesActions[J]) <> 0 then
            begin
              Indices := True;
              Break;
            end;
            inc(J);
          end;
          if Indices then Break;
        end;
        J := 0;
        if not Indices then while J < ActionsDebut.LesActions.Count do
        begin
          if Pos('Indice', ActionsDebut.LesActions[J]) <> 0 then
          begin
            Indices := True;
            Break;
          end;
          inc(J);
        end;
        if Indices then Indices :=
          ShowMes('Activation des indices', 'Ce labyrinthe propose certains indices.'+#10+
                  'Voulez-vous les activer ?', MB_YESNO or MB_ICONQUESTION) = ID_YES;
      end; // if not Indices
      UnPoint.X := FPosition.LaCase.X*7 + FPosition.LaPosition.X;
      UnPoint.Y := FPosition.LaCase.Y*7 + FPosition.LaPosition.Y;
      UnPoint.Z := FPosition.LaCase.Z;
      ActionsDebut.Execute(UnPoint, ABouge);
      FPosition.LaCase.X     := UnPoint.X div 7;
      FPosition.LaCase.Y     := UnPoint.Y div 7;
      FPosition.LaCase.Z     := UnPoint.Z;
      FPosition.LaPosition.X := UnPoint.X mod 7;
      FPosition.LaPosition.Y := UnPoint.Y mod 7;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Finalisation globale'}
  SL.Free;
  EnTete.Free;
  {$ENDREGION}

  {$REGION 'Marquage de "Ouvert"}
  FNouveau := False;
  {$ENDREGION}
end;

constructor TLabyrinthe.CreateNew(Colonnes, Lignes, Etages : integer; Perso : boolean);
var I, J : integer;
    Str0, Str2, StrB : string;
    DLL : THandle;
    NouveauLab : function(Cols, Rows, Etas : integer) : boolean;
    LesActions : TStringList;
    OK : boolean;
label NoPerso, ApresNoPerso;
begin
  if Application.Tag <> 2 then exit;
  inherited Create;

  {$REGION 'Initialisation globale'}
  Init;
  FConstruction := True;
  FNomLab := 'Nouveau Labyrinthe';
  Reponse := 0;
  AGagne := False;
  SLEnteteBoutons := TScStrings.Create;
  SLEnteteTeleporteurs := TScStrings.Create;
  ListeActionsZone := TScStrings.Create;
  if not Perso then goto NoPerso;
  DLL := LoadLibrary(PChar(Dir+'NouvLab.dll'));
  if DLL = 0 then
  begin
    ShowMes('Erreur', 'Impossible de charger le paramètreur', MB_OK or MB_ICONERROR);
    goto NoPerso;
  end;
  @NouveauLab := GetProcAddress(DLL, 'Nouveau');
  if @NouveauLab <> nil then
  begin
    OK := NouveauLab(Colonnes, Lignes, Etages);
    FreeLibrary(DLL);
    if OK then
    begin
      FLabyrinthe := TScStrings.CreateFromFile(Dir+'Labyrinthe');
      LesActions := TStringList.Create;
      LesActions.LoadFromFile(Dir+'Actions');
      LoadActions(LesActions);
      LesActions.Free;
      goto ApresNoPerso;
    end;
  end else ShowMes('Erreur', 'Impossible de charger le paramètreur', MB_OK or MB_ICONERROR);
NoPerso :
  {$ENDREGION}

  {$REGION 'Initialisation de FLabyrinthe'}
  FLabyrinthe := TScStrings.Create;
  FLabyrinthe.Clear;
  FLabyrinthe.Add('[Labyrinthe]');
  I := 0;
  Str0 := '';
  Str2 := '';
  StrB := '';
  while I < Colonnes do
  begin
    Str0 := Str0+'0000000';
    Str2 := Str2+'2222222';
    StrB := StrB+'BBBBBBB';
    inc(I);
  end;
  Str0[1] := '2';
  Str0[Length(Str0)] := '2';
  FLabyrinthe := TScStrings.Create;
  FLabyrinthe.Clear;
  FLabyrinthe.Add('[Labyrinthe]');
  for I := 1 to Etages do
  begin
    if I > 1 then for J := 1 to 7 do FLabyrinthe.Add(StrB);
    FLabyrinthe.Add(Str2);
    for J := 1 to Lignes*7-2 do FLabyrinthe.Add(Str0);
    FLabyrinthe.Add(Str2);
  end;
ApresNoPerso :
  {$ENDREGION}

  {$REGION 'Initialisation de Dimensions'}
  FDimensions.Colonnes := Colonnes;
  FDimensions.Lignes := Lignes;
  FDimensions.Etages := Etages;
  {$ENDREGION}

  {$REGION 'Initialisation de Description'}
  FDescription := TStringList.Create;
  FDescription.Clear;
  {$ENDREGION}

  {$REGION 'Marquage "Nouveau"'}
  FNouveau := True;
  {$ENDREGION}
end;

constructor TLabyrinthe.CreateActions(FileName : string; StringList : TStrings);
var SL, EnTete : TScStrings;
    I, J : integer;
begin
  inherited Create;

  {$REGION 'Initialisation globale'}
  SL := TScStrings.Create;
  EnTete := TScStrings.Create;
  SLEnteteBoutons := TScStrings.Create;
  SLEnteteTeleporteurs := TScStrings.Create;
  Init;
  FFileName := FileName;
  FConstruction := True;
  Reponse := 0;
  AGagne := False;
  {$ENDREGION}

  {$REGION 'Initialisation de FLabyrinthe'}
  SL.LoadFromFile(FileName);
  I := SL.IndexOf('[Labyrinthe]');
  FLabyrinthe := TScStrings.Create;
  FLabyrinthe.CopyFrom(SL, I, SL.Count);
  EnTete.CopyFrom(SL, 0, I);
  EnTete.Add('[LaFinDeLEntete]');
  FNomLab := ExtractFileName(FileName);
  FNomLab := Copy(FNomLab, 1, Length(FNomLab)-Length(ExtractFileExt(FNomLab)));
  {$ENDREGION}

  {$REGION 'Vérification sur l''édition de sauvegardes'}
  if EnTete.IndexOf('[Sauvegarde]') <> -1 then
  begin
    EnTete.Free;
    SL.Free;
    raise EIsSauvegarde.Create('Ce fichier est une sauvegarde !');
  end;
  {$ENDREGION}

  {$REGION 'Initialisation de Dimensions'}
  I := EnTete.IndexOf('[Dimensions]');
  if I <> -1 then
  begin
    J := EnTete.FindAtPos('[', 1, I+1);
    SL.CopyFrom(EnTete, I+1, J-I-1);
    FixeInt(SL, 'Colonnes', FDimensions.Colonnes);
    FixeInt(SL, 'Lignes'  , FDimensions.Lignes);
    FixeInt(SL, 'Etages'  , FDimensions.Etages);
    repeat EnTete.Delete(I) until (Length(EnTete[I]) > 0) and
      (EnTete[I][1] = '[');
  end;
  {$ENDREGION}

  {$REGION 'Initialisation de EditInfos'}
  I := EnTete.IndexOf('[EditInfos]');
  if I <> -1 then
  begin
    J := EnTete.FindAtPos('[', 1, I+1);
    SL.CopyFrom(EnTete, I+1, J-I-1);
    if SL.IndexOf('PeutPasEditer') <> -1 then raise
      ECreateError.Create('Impossible d''éditer ce labyrinthe');
    FixeBool(SL, 'PeutEditer', FFileInfos.PeutEditer);
    repeat EnTete.Delete(I) until (Length(EnTete[I]) > 0) and
      (EnTete[I][1] = '[');
  end;
  {$ENDREGION}

  {$REGION 'Initialisation de Description'}
  I := EnTete.IndexOf('[Description]');
  FDescription := TStringList.Create;
  FDescription.Clear;
  if I <> -1 then
  begin
    J := EnTete.FindAtPos('[', 1, I+1);
    SL.CopyFrom(EnTete, I+1, J-I-1);
    FDescription.Assign(SL);
    repeat EnTete.Delete(I) until (Length(EnTete[I]) > 0) and
      (EnTete[I][1] = '[');
  end;
  {$ENDREGION}

  {$REGION 'Initialisation de Options (pour "Cases")'}
  I := EnTete.IndexOf('[Options]');
  FNomCases := 'Cases.bmp';
  if I <> -1 then
  begin
    J := EnTete.FindAtPos('[', 1, I+1);
    SL.CopyFrom(EnTete, I+1, J-I-1);
    FixeStr(SL, 'Cases', FNomCases);
  end;
  if FileExists(Dir+'Cases\'+FNomCases) then
    Cases.LoadFromFile(Dir+'Cases\'+FNomCases);
  {$ENDREGION}

  {$REGION 'Initialisation des actions (pour le style des boutons)'}
  LoadActions(EnTete);
  {$ENDREGION}

  {$REGION 'Finalisation globale'}
  SL.Free;
  EnTete.Delete(EnTete.Count-1);
  FEnTete := StringList;
  FEnTete.Assign(EnTete);
  EnTete.Free;
  {$ENDREGION}
end;

destructor TLabyrinthe.Destroy;
var I : integer;
    Temp : LongInt;
begin
  FLabyrinthe.Free;
  SLEnteteBoutons.Free;
  SLEnteteTeleporteurs.Free;
  FDescription.Free;
  for I := 1 to 45 do FBoutons[I].Free;
  for I := 1 to 30 do FTeleporteurs[I].Free;
  FActionsFin.Free;
  FActionsDebut.Free;
  if FConstruction then ListeActionsZone.Free else
  begin
    for I := 0 to FActionsZone.Count-1 do
    begin
      Temp := StrToIntDef(FActionsZone.ValueFromIndex[I], 0);
      if Temp <> 0 then TActionsZone(Temp).Free;
    end;
  end;
  inherited Destroy;
end;

procedure TLabyrinthe.Init;
var I : integer;
begin
  FFileName := '';
  FNomLab := 'Sans Nom';
  FDimensions.Colonnes := 0;
  FDimensions.Lignes := 0;
  FDimensions.Etages := 1;
  Bouees := 0;
  Planches := 0;
  ClesArgent := 0;
  ClesOr := 0;
  NoBarque := 0;
  CouleurPion := clBlue;
  Temporisation := 500;
  Indices := False;
  FPosition.LaCase.X := 0;
  FPosition.LaCase.Y := 0;
  FPosition.LaCase.Z := 1;
  FPosition.LaPosition.X := 1;
  FPosition.LaPosition.Y := 1;
  FFileInfos.PeutEditer := True;
  FFileInfos.Version := '1.0';
  FFileInfos.Sauvegarde := False;
  for I := 1 to 45 do FBoutons[I] := TCaseBouton.Create(Self);
  for I := 1 to 30 do FTeleporteurs[I] := TCaseTelepor.Create(Self);
  FActionsFin := TCaseFin.Create(Self);
  FActionsDebut :=  TActionsDebut.Create(Self);
  for I := 1 to 20 do Variables[I] := 0;
  for I := 0 to maxbord do Bord[I] := Vide;
  FEnTete := nil;
end;

procedure TLabyrinthe.Poursuivre(B : boolean);
begin
  ActiveCaseSuivante := B;
  Continuer := B;
end;

function TLabyrinthe.DefinitStyle(Defaut : TStyleBouton; Str : String) : TStyleBouton;
begin
  Result := Defaut;
  if Str = 'Perso'          then Result := sPerso       else
  if Str = 'Cache'          then Result := sCache       else
  if Pos('Info', Str) > 0   then Result := sBorneInfo   else
  if Str = 'Obstacle'       then Result := sEpreuve     else
  if Str = 'Direction'      then Result := sDirection   else
  if Defaut = sPoussoir then
  begin
    if Str = 'Commutateur'    then Result := sCommutateur else
    if Str = 'Objet'          then Result := sObjet
  end;
end;

function TLabyrinthe.StyleToStr(St : TStyleBouton) : String;
begin
  case St of
    sCommutateur : Result := 'Commutateur';
    sBorneInfo   : Result := 'BorneInfo';
    sCache       : Result := 'Cache';
    sPerso       : Result := 'Perso';
    sEpreuve     : Result := 'Obstacle';
    sObjet       : Result := 'Objet';
    sDirection   : Result := 'Direction';
    else Result := EmptyStr;
  end;
end;

procedure TLabyrinthe.LoadActions(LesActions : TStringList);
var Actions, SL, SL2, SL3 : TScStrings;
    I, J, N : integer;
    Str : string;
    Temp : TActionsZone;
begin
  Actions := TScStrings.Create;
  SL      := TScStrings.Create;
  SL2     := TScStrings.Create;
  SL3     := TScStrings.Create;
  Actions.Assign(LesActions);

  {$REGION 'Actions des boutons'}
  I := Actions.IndexOf('[Boutons]');
  if I <> -1 then
  begin
    J := Actions.FindAtPos('[', 1, I+1);
    SL.CopyFrom(Actions, I+1, J-I-1);
    SL.Add('/Fin');
    J := SL.FindAtPos('/');
    if J <> -1 then SLEnteteBoutons.CopyFrom(SL, 0, J);
    for N := 1 to 45 do
    begin
      J := SL.FindFirstWord('/Bouton'+IntToStr(N), 0);
      if J <> -1 then
      begin
        SL2.CopyFrom(SL, J+1, SL.FindAtPos('/', 1, J+1)-J-1);
        Boutons[N].LesActions := SL2;
        Str := GetXToken(SL[J], ' ', 2);
        with Boutons[N] do
        begin
          Style := DefinitStyle(sPoussoir, Str);
          if (Style in StylesPerso) then
          begin
            SL3.FromString(SL[J], ' ', '{}');
            if SL3.Count < 3 then Nom := '' else
              Nom := Copy(SL3[2], 2, Length(SL3[2])-2);
          end;
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Actions des téléporteurs'}
  I := Actions.IndexOf('[Teleporteurs]');
  if I <> -1 then
  begin
    J := Actions.FindAtPos('[', 1, I+1);
    SL.CopyFrom(Actions, I+1, J-I-1);
    SL.Add('/Fin');
    J := SL.FindAtPos('/');
    if J <> -1 then SLEnteteTeleporteurs.CopyFrom(SL, 0, J);
    for N := 1 to 30 do
    begin
      J := SL.FindFirstWord('/Teleporteur'+IntToStr(N), 0);
      if J <> -1 then
      begin
        SL2.CopyFrom(SL, J+1, SL.FindAtPos('/', 1, J+1)-J-1);
        Teleporteurs[N].LesActions := SL2;
        Str := GetXToken(SL[J], ' ', 2);
        with Teleporteurs[N] do
        begin
          Style := DefinitStyle(sTransporteur, Str);
          if (Style in StylesPerso) then
          begin
            SL3.FromString(SL[J], ' ', '{}');
            if SL3.Count < 3 then Nom := '' else
              Nom := Copy(SL3[2], 2, Length(SL3[2])-2);
          end;
        end;
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Actions de la case de fin'}
  I := Actions.IndexOf('[Fin]');
  if I <> -1 then
  begin
    J := Actions.FindAtPos('[', 1, I+1);
    SL.CopyFrom(Actions, I+1, J-I-1);
    FActionsFin.LesActions := SL;
  end;
  {$ENDREGION}

  {$REGION 'Actions de début de partie'}
  I := Actions.IndexOf('[Debut]');
  if I <> -1 then
  begin
    J := Actions.FindAtPos('[', 1, I+1);
    SL.CopyFrom(Actions, I+1, J-I-1);
    FActionsDebut.LesActions := SL;
  end;
  {$ENDREGION}

  {$REGION 'Actions effectuée à l''entrée d''une zone'}
  if FConstruction then ListeActionsZone := TScStrings.Create else
  begin
    FActionsZone := TStringList.Create;
  end;
  I := Actions.IndexOf('[Zones]');
  if I <> -1 then
  begin
    J := Actions.FindAtPos('[', 1, I+1);
    if FConstruction then ListeActionsZone.CopyFrom(Actions, I, J-1) else
    begin
      SL.CopyFrom(Actions, I+1, J-I-1);
      SL.Add('/Fin');
      J := SL.FindAtPos('/');
      while J < SL.Count-1 do
      begin
        Str := SL[J];
        I := J + 1;
        J := SL.FindAtPos('/', 1, I);
        if Copy(Str, 1, 6) <> '/Zone_' then Continue;
        SL2.CopyFrom(SL, I, J-I);
        Temp := TActionsZone.Create(Self);
        Temp.LesActions := SL2;
        FActionsZone.Values[Copy(Str, 7, Length(Str))] := IntToStr(LongInt(Temp));
      end;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Libération des variables'}
  Actions.Free;
  SL.Free;
  SL2.Free;
  SL3.Free;
  {$ENDREGION}
end;

procedure TLabyrinthe.SetLabyrinthe(X, Y, Z, SorteCase : integer);
var Str : String;
begin
  if Exterieur(X, Y, Z, false) then exit;
  inc(Y, (Z-1)*(FDimensions.Lignes+1)*7);
  Str := FLabyrinthe[Y+1];
  Str[X+1] := Char(SorteCase);
  FLabyrinthe[Y+1] := Str;
end;

function TLabyrinthe.GetLabyrinthe(X, Y, Z : integer) : integer;
begin
  if Exterieur(X, Y, Z, false) then
  begin
    Result := Bord[MinMax(Z, 1, maxbord)];
    exit;
  end;
  inc(Y, (Z-1)*(FDimensions.Lignes+1)*7);
  Result := Byte(FLabyrinthe[Y+1][X+1]);
end;

function TLabyrinthe.Exterieur(X, Y, Z : integer; bordinclus : boolean) : boolean;
var I : integer;
begin
  if bordinclus then I := 1 else I := 0;
  Result := (X < -I) or (Y < -I) or (Z < 1) or (X >= FDimensions.Colonnes*7+I) or
     (Y >= FDimensions.Lignes*7+I) or (Z > FDimensions.Etages);
end;

function TLabyrinthe.GetTouche : TFleche;
begin
  if TouchePressee then Result := Direction else Result := sansDirection;
end;

function TLabyrinthe.GetActionsZone(X, Y, Z : integer) : TActionsZone;
var Adresse : LongInt;
begin
  Adresse := StrToIntDef(FActionsZone.Values[IntToStr(X)+'_'+IntToStr(Y)+'_'+IntToStr(Z)], 0);
  if Adresse = 0 then Result := nil else Result := TActionsZone(Adresse);
end;

procedure TLabyrinthe.Sauvegarder(FileName : string; X, Y, Etage : integer; B, P, PC, GC, Barque : integer; Indices : boolean);
var SL : TStringList;
    I, J, Index : integer;
    Tmp : LongInt;
    Temp : TActionsZone;
    Str : string;
    Style : TStyleBouton;
begin
  SL := TStringList.Create;
  SL.Clear;
  SL.Add('[Dimensions]');
  SL.Add('Colonnes: '+IntToStr(FDimensions.Colonnes));
  SL.Add('Lignes: '+IntToStr(FDimensions.Lignes));
  SL.Add('Etages: '+IntToStr(FDimensions.Etages));
  SL.Add('[Options]');
  SL.Add('Cases: '+FNomCases);
  SL.Add('[Sauvegarde]');
  SL.Add('NomLabyrinthe: '+FNomLab);
  SL.Add('PositionX: '  +IntToStr(X));
  SL.Add('PositionY: '  +IntToStr(Y));
  SL.Add('Etage: '      +IntToStr(Etage));
  SL.Add('Bouees: '     +IntToStr(B));
  SL.Add('Planches: '   +IntToStr(P));
  SL.Add('PetitesCles: '+IntToStr(PC));
  SL.Add('GrandesCles: '+IntToStr(GC));
  SL.Add('Barque: '     +IntToStr(Barque));
  if CouleurPion <> clBlue then SL.Add('Couleur: $'   +IntToHex(CouleurPion, 6));
  if Temporisation <> 500  then SL.Add('Temporisation: '+IntToStr(Temporisation));
  SL.Add('Indices: '+BoolToStr(Indices));
  J := 45;
  repeat
    if Boutons[J].Compteur <> 0 then break;
    dec (J);
  until J = 0;
  if J > 0 then
  begin
    Str := 'CompteursBoutons:';
    for I := 1 to J do Str := Str + ' ' + IntToStr(Boutons[I].Compteur);
    SL.Add(Str);
  end;
  J := 30;
  repeat
    if Teleporteurs[J].Compteur <> 0 then break;
    dec (J);
  until J = 0;
  if J > 0 then
  begin
    Str := 'CompteursTeleporteurs:';
    for I := 1 to J do Str := Str + ' ' + IntToStr(Teleporteurs[I].Compteur);
    SL.Add(Str);
  end;
  if ActionsFin.Compteur > 0 then
    SL.Add('CompteurFin: '+IntToStr(ActionsFin.Compteur));
  J := 20;
  repeat
    if Variables[J] <> 0 then break;
    dec (J);
  until J = 0;
  if J > 0 then
  begin
    Str := 'Variables:';
    for I := 1 to J do Str := Str + ' ' + IntToStr(Variables[I]);
    SL.Add(Str);
  end;
  if FActionsZone.Count > 0 then
  begin
    Str := 'CompteursZones:';
    for I := 0 to FActionsZone.Count-1 do
    begin
      Tmp := StrToInt(FActionsZone.ValueFromIndex[I]);
      Str := Str + ' ' + IntToStr(TActionsZone(Tmp).Compteur);
    end;
    SL.Add(Str);
  end;
  Str := 'Bord:';
  J := maxbord;
  while (J > 1) and (Bord[J-1] = Bord[J]) do dec(J);
  for I := 1 to J do Str := Str+' '+Char(Bord[I]);
  SL.Add(Str);
  Index := SL.Count;
  I := 0;
  while I < FDescription.Count do
  begin
    SL.Add(FDescription[I]);
    inc(I);
  end;
  if SL.Count > Index then SL.Insert(Index, '[Description]');
  Index := SL.Count;
  for I := 1 to 45 do
  begin
    Style := Boutons[I].Style;
    if (Boutons[I].LesActions.Count = 0) and
       (Style = sPoussoir) then Continue;
    Str := '/Bouton'+IntToStr(I);
    if (Style <> sPoussoir) then Str := Str+' '+StyleToStr(Style);
    if (Style in StylesPerso) then
      Str := Str+' {'+Boutons[I].Nom+'}';
    SL.Add(Str);
    J := 0;
    while J < Boutons[I].LesActions.Count do
    begin
      SL.Add(Boutons[I].LesActions[J]);
      inc(J);
    end;
  end;
  if SL.Count > Index then SL.Insert(Index, '[Boutons]');
  Index := SL.Count;
  for I := 1 to 30 do
  begin
    Style := Teleporteurs[I].Style;
    if (Teleporteurs[I].LesActions.Count = 0) and
       (Style = sTransporteur) then Continue;
    Str := '/Teleporteur'+IntToStr(I);
    if (Style <> sTransporteur) then Str := Str+' '+StyleToStr(Style);
    if (Style in StylesPerso) then
      Str := Str+' {'+Teleporteurs[I].Nom+'}';
    SL.Add(Str);
    J := 0;
    while J < Teleporteurs[I].LesActions.Count do
    begin
      SL.Add(Teleporteurs[I].LesActions[J]);
      inc(J);
    end;
  end;
  if SL.Count > Index then SL.Insert(Index, '[Teleporteurs]');
  Index := SL.Count;
  I := 0;
  while I < FActionsFin.LesActions.Count do
  begin
    SL.Add(FActionsFin.LesActions[I]);
    inc(I);
  end;
  if SL.Count > Index then SL.Insert(Index, '[Fin]');
  Index := SL.Count;
  for I := 0 to FActionsZone.Count-1 do
  begin
    Tmp := StrToIntDef(FActionsZone.ValueFromIndex[I], 0);
    if Tmp = 0 then Continue;
    Temp := TActionsZone(Tmp);
    if Temp.LesActions.Count = 0 then Continue;
    SL.Add('/Zone_'+FActionsZone.Names[I]);
    for J := 0 to Temp.LesActions.Count-1 do
      SL.Add(Temp.LesActions[J]);
  end;
  if SL.Count > Index then SL.Insert(Index, '[Zones]');
  I := 0;
  while I < FLabyrinthe.Count do
  begin
    SL.Add(FLabyrinthe[I]);
    inc(I);
  end;
  SL.SaveToFile(FileName);
  SL.Free;
end;

procedure TLabyrinthe.Enregistrer(FileName : string);
var SL : TStringList;
    I, J, Index : integer;
    Str : string;
    Style : TStyleBouton;
begin
  if FNouveau then
  begin
    FNomLab := ExtractFileName(FileName);
    FNomLab := Copy(FNomLab, 1, Length(FNomLab)-Length(ExtractFileExt(FNomLab)));
  end;
  SL := TStringList.Create;
  SL.Clear;
  {Dimensions}
  SL.Add('[Dimensions]');
  SL.Add('Colonnes: '+IntToStr(FDimensions.Colonnes));
  SL.Add('Lignes: '+IntToStr(FDimensions.Lignes));
  SL.Add('Etages: '+IntToStr(FDimensions.Etages));
  {Description}
  Index := SL.Count;
  I := 0;
  while I < FDescription.Count do
  begin
    SL.Add(FDescription[I]);
    inc(I);
  end;
  if SL.Count > Index then SL.Insert(Index, '[Description]');
  {EditInfos}
  SL.Add('[EditInfos]');
  SL.Add(VersionEditeur);
  SL.Add('PeutEditer: Oui');
  {Options}
  Index := SL.Count;
  if LowerCase(FNomCases) <> 'cases.bmp' then SL.Add('Cases: '+FNomCases);
  SL.Add('NomLabyrinthe: '+FNomLab);
  if Indices then SL.Add('Indices: Oui');
  if SL.Count > Index then SL.Insert(Index, '[Options]');
  {Boutons}
  Index := SL.Count;
  for I := 0 to SLEnteteBoutons.Count-1 do
    SL.Add(SLEnteteBoutons[I]);
  for I := 1 to 45 do
  begin
    Style := Boutons[I].Style;
    if (Boutons[I].LesActions.Count = 0) and
       (Style = sPoussoir) then Continue;
    Str := '/Bouton'+IntToStr(I);
    if (Style <> sPoussoir) then Str := Str+' '+StyleToStr(Style);
    if (Style in StylesPerso) then
      Str := Str+' {'+Boutons[I].Nom+'}';
    SL.Add(Str);
    J := 0;
    while J < Boutons[I].LesActions.Count do
    begin
      SL.Add(Boutons[I].LesActions[J]);
      inc(J);
    end;
  end;
  if SL.Count > Index then SL.Insert(Index, '[Boutons]');
  {Teleporteurs}
  Index := SL.Count;
  for I := 0 to SLEnteteTeleporteurs.Count-1 do
    SL.Add(SLEnteteTeleporteurs[I]);
  for I := 1 to 30 do
  begin
    Style := Teleporteurs[I].Style;
    if (Teleporteurs[I].LesActions.Count = 0) and
       (Style = sTransporteur) then Continue;
    Str := '/Teleporteur'+IntToStr(I);
    if (Style <> sTransporteur) then Str := Str+' '+StyleToStr(Style);
    if (Style in StylesPerso) then
      Str := Str+' {'+Teleporteurs[I].Nom+'}';
    SL.Add(Str);
    J := 0;
    while J < Teleporteurs[I].LesActions.Count do
    begin
      SL.Add(Teleporteurs[I].LesActions[J]);
      inc(J);
    end;
  end;
  if SL.Count > Index then SL.Insert(Index, '[Teleporteurs]');
  {Fin}
  Index := SL.Count;
  I := 0;
  while I < FActionsFin.LesActions.Count do
  begin
    SL.Add(FActionsFin.LesActions[I]);
    inc(I);
  end;
  if SL.Count > Index then SL.Insert(Index, '[Fin]');
  {Debut}
  Index := SL.Count;
  I := 0;
  while I < FActionsDebut.LesActions.Count do
  begin
    SL.Add(FActionsDebut.LesActions[I]);
    inc(I);
  end;
  if SL.Count > Index then SL.Insert(Index, '[Debut]');
  {Zones}
  for I := 0 to ListeActionsZone.Count-1 do
    SL.Add(ListeActionsZone[I]);
  {Labyrinthe}
  I := 0;
  while I < FLabyrinthe.Count do
  begin
    SL.Add(FLabyrinthe[I]);
    inc(I);
  end;
  SL.SaveToFile(FileName);
  SL.Free;
  FFileName := FileName;
  FNouveau := False; //4.3
end;

procedure TLabyrinthe.EnregistrerActions(FileName : string);
var SL, LEnTete : TStringList;
    I, J, Index : integer;
    Str : string;
begin
  FNomLab := ExtractFileName(FileName);
  FNomLab := Copy(FNomLab, 1, Length(FNomLab)-Length(ExtractFileExt(FNomLab)));
  LEnTete := TStringList.Create;
  for I := 0 to FEnTete.Count-1 do
    begin
      Str := Trim(FEnTete[I]);
      repeat
        J := Pos('  ', Str);
        if J > 0 then delete (Str, J, 1);
      until J = 0;
      repeat
        J := Pos('[ ', Str);
        if J > 0 then delete (Str, J+1, 1);
      until J = 0;
      repeat
        J := Pos(' ]', Str);
        if J > 0 then delete (Str, J, 1);
      until J = 0;
      LEnTete.Add(Str);
    end;
  SL := TStringList.Create;
  SL.Add('[Dimensions]');
  SL.Add('Colonnes: '+IntToStr(FDimensions.Colonnes));
  SL.Add('Lignes: '+IntToStr(FDimensions.Lignes));
  SL.Add('Etages: '+IntToStr(FDimensions.Etages));
  Index := SL.Count;
  I := 0;
  while I < FDescription.Count do
  begin
    SL.Add(FDescription[I]);
    inc(I);
  end;
  if SL.Count > Index then SL.Insert(Index, '[Description]');
  SL.Add('[EditInfos]');
  SL.Add(VersionEditeur);
  SL.Add('PeutEditer: '+BoolToStr(FFileInfos.PeutEditer));
  I := 0;
  while I < LEnTete.Count do
  begin
    SL.Add(LEnTete[I]);
    inc(I);
  end;
  I := 0;
  while I < FLabyrinthe.Count do
  begin
    SL.Add(FLabyrinthe[I]);
    inc(I);
  end;
  SL.SaveToFile(FileName);
  SL.Free;
  FFileName := FileName;
end;

function TLabyrinthe.ExecuteBouton(ID : integer; var Ici : T3DPoint) : boolean;
begin
  Boutons[ID].Execute(Ici, Result);
end;

function TLabyrinthe.ExecuteTeleporteur(ID : integer; var Ici : T3DPoint) : boolean;
begin
  if Teleporteurs[ID].Execute(Ici, Result) then
    if (Teleporteurs[ID].Style = sTransporteur) then
    begin
      case ID of
        01..13 : Teleporteurs[ID].AllerA('Suivant '  +Char(ID+96), Ici);
        14..26 : Teleporteurs[ID].AllerA('Precedent '+Char(ID+96), Ici);
        27..30 : Teleporteurs[ID].AllerA('Aleatoire '+Char(ID+96), Ici);
      end;
      Result := True;
      Sleep(Temporisation);
    end;
end;

procedure TLabyrinthe.MessageFin(var Ici : T3DPoint; TypeFin : integer);
var ABouge : boolean;
begin
  if FActionsFin.Execute(Ici, ABouge) then
  case TypeFin of
    Vide : ShowMes('Gagné !', 'BRAVO ! Tu as vaincu le labyrinthe !',
                   MB_OK or MB_ICONINFORMATION);
    Tresor : ShowMes('Gagné !', 'BRAVO ! Tu as trouvé le trésor !',
                     MB_OK or MB_ICONINFORMATION);
  end;
end;

procedure TLabyrinthe.ExecuteChangeDeZone(var Ici : T3DPoint; X, Y, Z : integer);
var ABouge : boolean;
begin
  if ActionsZone[X, Y, Z] <> nil then
    ActionsZone[X, Y, Z].Execute(Ici, ABouge);
end;

function TLabyrinthe.ActionsZoneExists(X, Y, Z : integer) : boolean;
begin
  Result := ActionsZone[X, Y, Z] <> nil;
end;

function TLabyrinthe.StyleBouton(Code : integer) : TStyleBouton;
var NoBouton, Cmpt : integer;
begin
  if (Code in Bouton) then
  begin
    NoBouton :=  CodeToNoBouton(Code);
    Result := Boutons[NoBouton].Style;
    Cmpt := Boutons[NoBouton].Compteur;
  end
  else if (Code in Teleporteur) then
  begin
    NoBouton := Code-96;
    Result := Teleporteurs[NoBouton].Style;
    Cmpt := Teleporteurs[NoBouton].Compteur;
  end else
  begin
    Result := sPasBouton;
    Cmpt := 0;
  end;

  if (Result = sCommutateur) then
      Result := IIF((Cmpt mod 2) = 0, sCommutateurOff, sCommutateurOn);
end;

function TLabyrinthe.CodeToImage(Code : integer) : TBitmap;
var NoBouton : integer;
begin
  if (Code in Bouton) then
  begin
    NoBouton :=  CodeToNoBouton(Code);
    Result := Boutons[NoBouton].Image;
  end
  else if (Code in Teleporteur) then
  begin
    NoBouton := Code-96;
    Result := Teleporteurs[NoBouton].Image
  end
  else Result := nil;
end;

initialization
  Randomize;
  SetLength(TrueBoolStrs, 6);
  TrueBoolStrs[0] := 'Oui';
  TrueBoolStrs[1] := 'O';
  TrueBoolStrs[2] := 'Yes';
  TrueBoolStrs[3] := 'Y';
  TrueBoolStrs[4] := 'True';
  TrueBoolStrs[5] := 'T';
  SetLength(FalseBoolStrs, 5);
  FalseBoolStrs[0] := 'Non';
  FalseBoolStrs[1] := 'N';
  FalseBoolStrs[2] := 'No';
  FalseBoolStrs[3] := 'False';
  FalseBoolStrs[4] := 'F';
{-----Initialisation de Cases-----}
  Cases := TBitmap.Create;
  FNomCases := 'Cases.bmp';
  try
    Cases.LoadFromFile(Dir+'Cases\'+FNomCases);
  except
    ShowDialog('Erreur fatale', 'Impossible de charger le fichier Cases.bmp',
      dtError);
    raise;
  end;
finalization
{-----Libération des variables globales-----}
  Cases.Free;
end.
