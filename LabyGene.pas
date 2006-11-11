{*
  Unité principale de GeneLaby.exe
  Cette unité contient la fiche principale de GeneLaby.exe.
  @author Jean-Paul Doeraene
  @version 5.0
*}
unit LabyGene;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, Math, ExtCtrls, ComCtrls, ScUtils;

type
  {*
    Fiche principale de GeneLaby.exe
    @author Jean-Paul Doeraene
    @version 5.0
  *}
  TFormPrincipale = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    EditNomFichier: TEdit;
    EditLignes: TSpinEdit;
    EditColonnes: TSpinEdit;
    EditEtages: TSpinEdit;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    EditBlocage: TSpinEdit;
    CBCarrefours: TCheckBox;
    EditBlocEscaliers: TSpinEdit;
    EditBlocBoucles: TSpinEdit;
    EditPeriode: TSpinEdit;
    RGModulation: TRadioGroup;
    Panel1: TPanel;
    BExe: TButton;
    BAide: TButton;
    BQuitter: TButton;
    Label6: TLabel;
    procedure BAideClick(Sender: TObject);
    procedure BExeClick(Sender : TObject);
    procedure BQuitterClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGModulationClick(Sender: TObject);
    procedure EditLignesChange(Sender: TObject);
    procedure EditColonnesChange(Sender: TObject);
    procedure EditEtagesChange(Sender: TObject);
  private
    { Déclarations privées }
    procedure initMID;
  public
    { Déclarations publiques }
  end; //TForm1

  {*
    Représente une case du labyrinthe
    @author Jean-Paul Doeraene
    @version 5.0
  *}
  TCase = class
  private
    X, Y, Z : integer;
    bEsca, bBoucle, bDoitCombler : boolean;
    ordre : array[0..5] of smallint;
  public
    dir : array[0..5] of boolean;
    libre : boolean;
    car : char;
    Niveau : integer;
    constructor Create (XX, YY, ZZ : integer);
    function ACote(d : smallint) : TCase;
    procedure Comble(dd : smallint; nn : integer; cc : char);
    function PeutTraverser(dd : smallint) : boolean;
    function PeutPoserEscalier(dd : smallint) : boolean;
    function PeutBoucler(dd : smallint) : boolean;
  end;

var
  FormPrincipale: TFormPrincipale; /// Instance de la fiche principale

  laby : array [0..29,0..29,0..9] of TCase;
  NLig, NCol, NEta : Integer;
  NLigZ, NColZ, NEtaZ : Integer;
  Xdep, Ydep, Zdep, Xmn, Ymn, Zmn : Integer;
  Labyrinthe : TStringList;
  Total, GrandTotal, Mid : integer;
  PBlocage, PBlocEscaliers, PBlocBoucles : integer;
  IIModulation : integer;
  bCarrefours : boolean;
  NomComplet : String;

procedure Principale;

implementation

{$R *.DFM}

const {don't localize}
  /// Contenu générique du fichier projet d'un labyrinthe généré
  sMasterFileContents =
  '<?xml version="1.0" encoding="UTF-8"?>'#10+
  '<funlabyrinthe version="%s">'#10+
  '  <title>Labyrinthe genere</title>'#10+
  '  <description>Labyrinthe genere</description>'#10+
  '  <difficulty>Moyen</difficulty>'#10+
  '  <author id="2">Jean-Paul Doeraene</author>'#10+
  '  <units><unit type="application/bpl" href="FunLabyCore.bpl"/></units>'#10+
  '  <maps><map id="MainMap" type="application/flm" href="%s" maxviewsize="%d"/></maps>'#10+
  '  <players>'#10+
  '    <player id="Player1">'#10+
  '      <position map="MainMap" posx="%d" posy="%d" posz="%d"/>'#10+
  '    </player>'#10+
  '  </players>'#10+
  '</funlabyrinthe>'#10;

  /// Représentation textuelle de la version actuelle
  sCurrentVersion = '5.0';

  /// Code de format d'un fichier FLM (correspond à '.flm')
  FLMFormatCode : LongInt = $6D6C662E;

  FLMVersion = 1; /// Version courante du format FLM

function WDir : string;
//var PS : string;
begin
//  PS := ParamStr(0);
//  Result := ExtractFilePath(PS);
  Result := 'C:\Documents and Settings\All Users\Application Data\' +
    'SJRDoeraene\FunLabyrinthe\';
end;

procedure CreeSortie(Labyrinthe : TStrings;
  const MasterFileName, MapHRef, MapFileName : TFileName;
  DimX, DimY, DimZ : integer; MaxViewSize : integer = 1);
var PosX, PosY, PosZ, X, Y, Z, Value : integer;
    Stream : TStream;
begin
  ForceDirectories(ExtractFilePath(MapFileName));

  PosX := 0;
  PosY := 0;
  PosZ := 0;

  Stream := TFileStream.Create(MapFileName, fmCreate or fmShareExclusive);
  with Stream do
  try
    Value := FLMFormatCode;
    WriteBuffer(Value, 4);

    Value := FLMVersion;
    WriteBuffer(Value, 4);

    // Taille de zone
    Value := 7;
    WriteBuffer(DimX, 4);
    WriteBuffer(DimY, 4);
    WriteBuffer(DimZ, 4);
    WriteBuffer(Value, 4);
    WriteBuffer(Value, 4);

    Value := 7; // taille de palette
    WriteBuffer(Value, 4);
    WriteStrToStream(Stream, 'Grass--');
    WriteStrToStream(Stream, 'Wall--');
    WriteStrToStream(Stream, 'Grass-Crossroads-');
    WriteStrToStream(Stream, 'Grass-UpStairs-');
    WriteStrToStream(Stream, 'Grass-DownStairs-');
    WriteStrToStream(Stream, 'Grass-Treasure-');
    WriteStrToStream(Stream, 'Grass-Outside-');

    for Z := 0 to DimZ-1 do for Y := 0 to DimY-1 do for X := 0 to DimX-1 do
    begin
      case Labyrinthe[Z*(DimY+7) + Y][X+1] of
        '0' : Value := 0;
        '2' : Value := 1;
        ':' : Value := 2;
        '>' : Value := 3;
        '<' : Value := 4;
        ';' : Value := 5;
        'A' :
        begin
          Value := 0;
          PosX := X;
          PosY := Y;
          PosZ := Z;
        end;
      end;
      WriteBuffer(Value, 1);
    end;

    Value := 6;
    for Z := 0 to DimZ-1 do
      WriteBuffer(Value, 1);
  finally
    Free;
  end;

  with TStringList.Create do
  try
    Text := Format(sMasterFileContents,
      [sCurrentVersion, UTF8Encode(MapHRef), MaxViewSize, PosX, PosY, PosZ]);
    SaveToFile(MasterFileName);
  finally
    Free;
  end;
end;

constructor TCase.Create(XX, YY, ZZ : integer);
var I, J, Temp : smallint;
begin
  inherited Create;
  X := XX; Y := YY; Z := ZZ;
  Libre := true;
  Car := '2';
  for I := 0 to 5 do dir[I] := false;
  bEsca := (random(100) >= pBlocEscaliers);
  bBoucle := (random(100) >= pBlocBoucles);
  for I := 0 to 5 do ordre[I] := I;
  for I := 5 downto 1 do
  begin
    J := random (I+1);
    Temp := ordre[I];
    ordre[I] := ordre[J];
    ordre[J] := Temp;
  end;
end;

function oppose(dd : smallint) : smallint;
begin
  if (dd < 4) then result := (dd+2) mod 4
              else result := 9 - dd
end;

function TCase.ACote(d : smallint) : TCase;
begin
  case d of
    0 : if (Y > 0) then result := laby[X,Y-1,Z] else result := nil;
    1 : if (X < NCol-1) then result := laby[X+1,Y,Z] else result := nil;
    2 : if (Y < NLig-1) then result := laby[X,Y+1,Z] else result := nil;
    3 : if (X > 0) then result := laby[X-1,Y,Z] else result := nil;
    4 : if (Z < NEta-1) then result := laby [X,Y,Z+1] else result := nil;
    5 : if (Z > 0) then result := laby [X,Y,Z-1] else result := nil;
    else Result := nil;
  end;
end;

function TCase.PeutBoucler (dd : smallint) : boolean;
//dd est la direction d'où on vient
begin
  if Libre then result := true
  else if (not bBoucle) then result := false
  else result := (niveau = ACote(dd).niveau+1) and (not dir[dd])
end;

function TCase.PeutTraverser(dd : smallint) : boolean;
//dd est la direction où on va
var Apres : TCase;
begin
  Apres := ACote(dd);
  if (not bCarrefours) then result := false
  else if (Car <> '0') then result := false
  else if (Apres = nil) then result := false
  else if (not dir[(dd+1) mod 4]) or (not dir[(dd+3) mod 4]) or (dir[dd]) or (dir[(dd+2) mod 4]) then result := false
  //else if Apres.Libre then result := true
  else if Apres.PeutBoucler(oppose(dd)) then result := true
  else result := Apres.PeutTraverser(dd);
end;

function TCase.PeutPoserEscalier(dd : smallint) : boolean;
//dd est la direction d'où on vient
var Apres : TCase;
    I : smallint;
begin
  result := false;
  if (not bEsca) then exit;
  if (not Libre) and (not PeutBoucler(dd)) then exit;
  for I := 0 to 3 do
  begin
    Apres := ACote(i);
    if Apres = nil then continue;
    if Apres.Libre or Apres.PeutTraverser(I) then begin result := true; break; end;
  end;
end;

function blocage(nn : integer) : boolean;
var Temp : Extended;
begin
  case IIModulation of
    0 : Temp := arctan((nn-Mid)*4/Mid)/pi +0.5 ;
    1 : Temp := (1.0 - cos(2*pi*nn/Mid)) / 2;
    else Temp := 0.0;
  end;
  Temp := (GrandTotal-Total)* PBlocage / GrandTotal * Temp;
  result := random(100) < Temp;
end;

procedure TCase.Comble (dd : smallint; nn : integer; cc : char);
var index, D : smallint;
    Temp : TCase;
    CarSuivant : char;
begin
  if (Car <> '<') and (Car <> '>') then Car := cc;
  if (dd <> -1) then dir[dd] := true;
  if Car = ':' then
  begin
    D := (dd + 2) mod 4;
    Temp := ACote(D);
    dir[D] := true;
    if Temp.PeutBoucler(dd) then CarSuivant := '0' else CarSuivant := ':';
    Temp.Comble(dd, nn+1, CarSuivant);
    exit;
  end; // C = ':'
  if Libre then
  begin
    inc(Total);
    niveau := nn;
    Libre := false;
  end;
  bDoitCombler := (cc = '<') or (cc = '>');
  for index := 0 to 5 do
  begin
    D := ordre[index];
    CarSuivant := '0';
    Temp := Acote(D);
    if (Temp = nil) then continue;
    if (not bDoitCombler) then if blocage(niveau) then continue;
    if (D = 4) or (D = 5) then
    begin
      if (car <> '0') or (not Temp.PeutPoserEscalier(oppose(D))) then continue;
      case D of
        4 : begin Car := '>' ; CarSuivant := '<'; end;
        5 : begin Car := '<' ; CarSuivant := '>'; end;
      end; //case
    end
    else if not (Temp.Libre) then
    begin //ça pourrait devenir un carrefour ou boucle
      if Temp.PeutTraverser(D) then CarSuivant := ':'
      else if Temp.PeutBoucler(oppose(D)) then (* rien *)
      else continue;
    end;
    dir[D] := true;
    bDoitCombler := false;
    Temp.Comble(oppose(D), niveau+1, CarSuivant);
  end; //for
  if (niveau > laby[Xmn,Ymn,Zmn].niveau) and (Car = '0') then
  begin
    Xmn := X;
    Ymn := Y;
    Zmn := Z;
  end;
end;

procedure Principale;
var I, XX, YY, ZZ, CoordX, CoordY, CoordZ : integer;
    Temp, Temp2, TempB : String;
begin
  randomize;
  for YY := 0 to NLig-1 do for XX := 0 to NCol-1 do for ZZ := 0 to NEta-1 do
  begin
    laby[XX,YY,ZZ] := TCase.Create(XX,YY,ZZ);
  end;
  Total := 0;  GrandTotal := NLig*NCol*NEta;
  //Mid := (NLig + NCol + NEta) div 3;
  Xdep := random (NCol); Ydep :=random(NLig); Zdep := random(NEta);
  Xmn := Xdep; Ymn := Ydep; Zmn := Zdep;
  laby[Xdep,Ydep,Zdep].Comble(-1, 0, 'A');
  Labyrinthe := TStringList.Create;
  Labyrinthe.Add('[Labyrinthe]');
  Temp2 := EmptyStr;
  for CoordX := 1 to NColZ do Temp2 := Concat(Temp2, '2222222');
  TempB := EmptyStr;
  for CoordX := 1 to NColZ do TempB := Concat(TempB, 'BBBBBBB');
  for CoordZ := 1 to NEtaZ do
  begin
    for CoordY := 1 to NLigZ do
    begin
      for I := 1 to 7 do Labyrinthe.Add(Temp2);
    end;
    if CoordZ < NEtaZ then
    begin
      for I := 1 to 7 do Labyrinthe.Add(TempB);
    end;
  end;
  laby[Xmn, Ymn, Zmn].Car := ';';
  for XX := 0 to NCol-1 do for YY := 0 to NLig-1 do for ZZ := 0 to NEta-1 do
  begin
    CoordX := XX*2 + 2 + (XX div 3);
    CoordY := YY*2 + 2 + (YY div 3) + ZZ*7*(NLigZ+1);
    Temp := Labyrinthe[CoordY];
    Temp[CoordX] := laby[XX,YY,ZZ].Car;
    if laby[XX,YY,ZZ].dir[3] then Temp[CoordX-1] := '0';
    if laby[XX,YY,ZZ].dir[1] then Temp[CoordX+1] := '0';
    Labyrinthe[CoordY] := Temp;
    if laby[XX,YY,ZZ].dir[0] then
    begin
      Temp := Labyrinthe[CoordY-1];
      Temp[CoordX] := '0';
      Labyrinthe[CoordY-1] := Temp;
    end;
    if laby[XX,YY,ZZ].dir[2] then
    begin
      Temp := Labyrinthe[CoordY+1];
      Temp[CoordX] := '0';
      Labyrinthe[CoordY+1] := Temp;
    end;
  end;
(*  Labyrinthe.Insert(0, 'Etages: '+IntToStr(NEtaZ));
  Labyrinthe.Insert(0, 'Lignes: '+IntToStr(NLigZ));
  Labyrinthe.Insert(0, 'Colonnes: '+IntToStr(NColZ));
  Labyrinthe.Insert(0, '[Dimensions]');
  {$I-} MkDir(WDir+'Labyrinthes'); {$I+}*)
  Labyrinthe.Delete(0);
  NomComplet := WDir+'Labyrinths\'+FormPrincipale.EditNomFichier.Text;
  CreeSortie(Labyrinthe, NomComplet+'.flg',
    FormPrincipale.EditNomFichier.Text+'\MainMap.flm',
    NomComplet+'\MainMap.flm',
    NColZ*7, NLigZ*7, NEtaZ);
//  Labyrinthe.SaveToFile(NomComplet);
  Labyrinthe.Free;
end;

procedure TFormPrincipale.initMID;
begin
  EditPeriode.Value := EditLignes.Value+EditColonnes.Value+(EditEtages.Value div 3);
end;

procedure TFormPrincipale.BExeClick(Sender: TObject);
begin
  if EditNomFichier.Text = '' then EditNomFichier.Text := 'LabyGene';
  NLigZ := EditLignes  .Value; NLig := NLigZ * 3;
  NColZ := EditColonnes.Value; NCol := NColZ * 3;
  NEtaZ := EditEtages.Value;   NEta := NEtaZ;
  IIModulation := RGModulation.ItemIndex;
  PBlocage := EditBlocage.Value;
  Mid := EditPeriode.Value;
  PBlocEscaliers := EditBlocEscaliers.Value;
  bCarrefours := CBCarrefours.Checked;
  PBlocBoucles := EditBlocBoucles.Value;
  Principale;
  ShowMessage('Labyrinthe de profondeur '+IntToStr(laby[Xmn,Ymn,Zmn].niveau)+' créé.');
  BQuitter.Enabled := True;
  BQuitter.SetFocus;
end;

procedure TFormPrincipale.BAideClick(Sender : TObject);
begin
  Application.HelpContext(1);
end;

procedure TFormPrincipale.BQuitterClick(Sender: TObject);
begin
  if FileExists(WDir+'Labyrinthe.exe') then
    WinExec(PChar(WDir+'Labyrinthe.exe "'+NomComplet+'"'), SW_ShowNormal);
  Close;
end;

procedure TFormPrincipale.FormCreate(Sender: TObject);
begin
  Application.HelpFile := WDir+'GeneLaby.hlp';
end;

procedure TFormPrincipale.RGModulationClick(Sender: TObject);
begin
  if (RGModulation.ItemIndex = 1) then
  begin
    EditBlocage.Value := 80;
  end
  else
  begin
    EditBlocage.Value := 60;
  end;
end;

procedure TFormPrincipale.EditLignesChange(Sender: TObject);
begin
  initMID;
end;

procedure TFormPrincipale.EditColonnesChange(Sender: TObject);
begin
  initMID;
end;

procedure TFormPrincipale.EditEtagesChange(Sender: TObject);
begin
  initMID;
end;

end.

