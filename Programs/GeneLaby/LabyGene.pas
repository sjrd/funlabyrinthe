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
  StdCtrls, Spin, Math, ExtCtrls, ComCtrls, IniFiles, ScUtils, FunLabyUtils,
  FilesUtils, UnitFiles, SepiReflectionCore;

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
    procedure BExeClick(Sender: TObject);
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
  end;

  {*
    Représente une case du labyrinthe
    @author Jean-Paul Doeraene
    @version 5.0
  *}
  TCase = class
  private
    X, Y, Z: Integer;
    bEsca, bBoucle, bDoitCombler: Boolean;
    ordre: array[0..5] of Smallint;
  public
    dir: array[0..5] of Boolean;
    libre: Boolean;
    car: char;
    Niveau: Integer;
    constructor Create(XX, YY, ZZ: Integer);
    function ACote(d: Smallint): TCase;
    procedure Comble(dd: Smallint; nn: Integer; cc: char);
    function PeutTraverser(dd: Smallint): Boolean;
    function PeutPoserEscalier(dd: Smallint): Boolean;
    function PeutBoucler(dd: Smallint): Boolean;
  end;

var
  FormPrincipale: TFormPrincipale; /// Instance de la fiche principale

  laby: array [0..29, 0..29, 0..9] of TCase;
  NLig, NCol, NEta: Integer;
  NLigZ, NColZ, NEtaZ: Integer;
  Xdep, Ydep, Zdep, Xmn, Ymn, Zmn: Integer;
  Labyrinthe: TStringList;
  Total, GrandTotal, Mid: Integer;
  PBlocage, PBlocEscaliers, PBlocBoucles: Integer;
  IIModulation: Integer;
  bCarrefours: Boolean;
  NomLabyrinthe: String;

procedure Principale;

implementation

{$R *.DFM}

procedure CreeSortie(Labyrinthe: TStrings; const MasterFileName: TFileName;
  DimX, DimY, DimZ: Integer; MaxViewSize: Integer = 1);
var
  UnitFileDescs: TUnitFileDescs;
  MasterFile: TMasterFile;
  Master: TMaster;
  Player: TPlayer;
  Map: TMap;
  Grass, Wall, Crossroads, UpStairs, DownStairs, Treasure, Outside: TSquare;
  X, Y, Z: Integer;
  Pos: T3DPoint;
begin
  SetLength(UnitFileDescs, 1);
  UnitFileDescs[0].HRef := 'FunLabyBase.bpl';

  MasterFile := nil;
  try
    MasterFile := TMasterFile.CreateNew(nil, UnitFileDescs);
    Master := MasterFile.Master;
    Player := TPlayer.Create(Master, 'Player', 'Joueur');
    Map := TMap.Create(Master, 'MainMap', Point3D(DimX, DimY, DimZ), 7, 7);
    Map.MaxViewSize := MaxViewSize;

    with MasterFile do
    begin
      Title := 'Labyrinthe généré';
      Description := Title;
      Difficulty := Format('%dx%dx%d', [DimX, DimY, DimZ]);
      Author := 'Générateur écrit par Jean-Paul Doeraene';
    end;

    Grass := Master.Square['Grass---'];
    Wall := Master.Square['Wall---'];
    Crossroads := Master.Square['Grass-Crossroads--'];
    UpStairs := Master.Square['Grass-UpStairs--'];
    DownStairs := Master.Square['Grass-DownStairs--'];
    Treasure := Master.Square['Grass-Treasure--'];
    Outside := Master.Square['Outside---'];

    for X := 0 to DimX-1 do
    begin
      for Y := 0 to DimY-1 do
      begin
        for Z := 0 to DimZ-1 do
        begin
          Pos := Point3D(X, Y, Z);

          case Labyrinthe[Z*(DimY+7) + Y][X+1] of
            '0': Map[Pos] := Grass;
            '2': Map[Pos] := Wall;
            ':': Map[Pos] := Crossroads;
            '>': Map[Pos] := UpStairs;
            '<': Map[Pos] := DownStairs;
            ';': Map[Pos] := Treasure;
            'A':
            begin
              Map[Pos] := Grass;
              Player.ChangePosition(Map, Pos);
            end;
          end;
        end;
      end;
    end;

    for Z := 0 to DimZ-1 do
      Map.Outside[Z] := Outside;

    MasterFile.Save(MasterFileName);
  finally
    MasterFile.Free;
  end;
end;

constructor TCase.Create(XX, YY, ZZ: Integer);
var
  I, J, Temp: Smallint;
begin
  inherited Create;
  X := XX;
  Y := YY;
  Z := ZZ;
  Libre := True;
  Car := '2';
  for I := 0 to 5 do
    dir[I] := False;
  bEsca := (random(100) >= pBlocEscaliers);
  bBoucle := (random(100) >= pBlocBoucles);
  for I := 0 to 5 do
    ordre[I] := I;
  for I := 5 downto 1 do
  begin
    J := random(I+1);
    Temp := ordre[I];
    ordre[I] := ordre[J];
    ordre[J] := Temp;
  end;
end;

function oppose(dd: Smallint): Smallint;
begin
  if (dd < 4) then
    Result := (dd+2) mod 4
  else
    Result := 9 - dd;
end;

function TCase.ACote(d: Smallint): TCase;
begin
  case d of
    0: if (Y > 0) then
        Result := laby[X, Y-1, Z]
      else
        Result := nil;
    1: if (X < NCol-1) then
        Result := laby[X+1, Y, Z]
      else
        Result := nil;
    2: if (Y < NLig-1) then
        Result := laby[X, Y+1, Z]
      else
        Result := nil;
    3: if (X > 0) then
        Result := laby[X-1, Y, Z]
      else
        Result := nil;
    4: if (Z < NEta-1) then
        Result := laby[X, Y, Z+1]
      else
        Result := nil;
    5: if (Z > 0) then
        Result := laby[X, Y, Z-1]
      else
        Result := nil;
  else
    Result := nil;
  end;
end;

function TCase.PeutBoucler(dd: Smallint): Boolean;
  // dd est la direction d'où on vient
begin
  if Libre then
    Result := True
  else if (not bBoucle) then
    Result := False
  else
    Result := (niveau = ACote(dd).niveau+1) and (not dir[dd]);
end;

function TCase.PeutTraverser(dd: Smallint): Boolean;
  // dd est la direction où on va
var
  Apres: TCase;
begin
  Apres := ACote(dd);
  if (not bCarrefours) then
    Result := False
  else if (Car <> '0') then
    Result := False
  else if (Apres = nil) then
    Result := False
  else if (not dir[(dd+1) mod 4]) or (not dir[(dd+3) mod 4]) or
    (dir[dd]) or (dir[(dd+2) mod 4]) then
    Result := False
  //else if Apres.Libre then result := true
  else if Apres.PeutBoucler(oppose(dd)) then
    Result := True
  else
    Result := Apres.PeutTraverser(dd);
end;

function TCase.PeutPoserEscalier(dd: Smallint): Boolean;
  // dd est la direction d'où on vient
var
  Apres: TCase;
  I: Smallint;
begin
  Result := False;
  if (not bEsca) then
    Exit;
  if (not Libre) and (not PeutBoucler(dd)) then
    Exit;
  for I := 0 to 3 do
  begin
    Apres := ACote(i);
    if Apres = nil then
      Continue;
    if Apres.Libre or Apres.PeutTraverser(I) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function blocage(nn: Integer): Boolean;
var
  Temp: Extended;
begin
  case IIModulation of
    0: Temp := arctan((nn-Mid)*4/Mid)/pi +0.5;
    1: Temp := (1.0 - cos(2*pi*nn/Mid)) / 2;
  else
    Temp := 0.0;
  end;
  Temp := (GrandTotal-Total)* PBlocage / GrandTotal * Temp;
  Result := random(100) < Temp;
end;

procedure TCase.Comble(dd: Smallint; nn: Integer; cc: char);
var
  Index, D: Smallint;
  Temp: TCase;
  CarSuivant: char;
begin
  if (Car <> '<') and (Car <> '>') then
    Car := cc;
  if (dd <> -1) then
    dir[dd] := True;
  if Car = ':' then
  begin
    D := (dd + 2) mod 4;
    Temp := ACote(D);
    dir[D] := True;
    if Temp.PeutBoucler(dd) then
      CarSuivant := '0'
    else
      CarSuivant := ':';
    Temp.Comble(dd, nn+1, CarSuivant);
    Exit;
  end; // C = ':'
  if Libre then
  begin
    Inc(Total);
    niveau := nn;
    Libre := False;
  end;
  bDoitCombler := (cc = '<') or (cc = '>');
  for Index := 0 to 5 do
  begin
    D := ordre[Index];
    CarSuivant := '0';
    Temp := Acote(D);
    if (Temp = nil) then
      Continue;
    if (not bDoitCombler) then
      if blocage(niveau) then
        Continue;
    if (D = 4) or (D = 5) then
    begin
      if (car <> '0') or (not Temp.PeutPoserEscalier(oppose(D))) then
        Continue;
      case D of
        4:
        begin
          Car := '>';
          CarSuivant := '<';
        end;
        5:
        begin
          Car := '<';
          CarSuivant := '>';
        end;
      end; //case
    end else if not (Temp.Libre) then
    begin //ça pourrait devenir un carrefour ou boucle
      if Temp.PeutTraverser(D) then
        CarSuivant := ':'
      else if Temp.PeutBoucler(oppose(D)) then (* rien *)
      else
        Continue;
    end;
    dir[D] := True;
    bDoitCombler := False;
    Temp.Comble(oppose(D), niveau+1, CarSuivant);
  end; //for
  if (niveau > laby[Xmn, Ymn, Zmn].niveau) and (Car = '0') then
  begin
    Xmn := X;
    Ymn := Y;
    Zmn := Z;
  end;
end;

procedure Principale;
var
  I, XX, YY, ZZ, CoordX, CoordY, CoordZ: Integer;
  Temp, Temp2, TempB: String;
begin
  randomize;
  for YY := 0 to NLig-1 do
    for XX := 0 to NCol-1 do
      for ZZ := 0 to NEta-1 do
        laby[XX, YY, ZZ] := TCase.Create(XX, YY, ZZ);
  Total := 0;
  GrandTotal := NLig*NCol*NEta;
  //Mid := (NLig + NCol + NEta) div 3;
  Xdep := random(NCol);
  Ydep := random(NLig);
  Zdep := random(NEta);
  Xmn := Xdep;
  Ymn := Ydep;
  Zmn := Zdep;
  laby[Xdep, Ydep, Zdep].Comble(-1, 0, 'A');
  Labyrinthe := TStringList.Create;
  Labyrinthe.Add('[Labyrinthe]');
  Temp2 := EmptyStr;
  for CoordX := 1 to NColZ do
    Temp2 := Concat(Temp2, '2222222');
  TempB := EmptyStr;
  for CoordX := 1 to NColZ do
    TempB := Concat(TempB, 'BBBBBBB');
  for CoordZ := 1 to NEtaZ do
  begin
    for CoordY := 1 to NLigZ do
    begin
      for I := 1 to 7 do
        Labyrinthe.Add(Temp2);
    end;
    if CoordZ < NEtaZ then
    begin
      for I := 1 to 7 do
        Labyrinthe.Add(TempB);
    end;
  end;
  laby[Xmn, Ymn, Zmn].Car := ';';
  for XX := 0 to NCol-1 do
    for YY := 0 to NLig-1 do
      for ZZ := 0 to NEta-1 do
      begin
        CoordX := XX*2 + 2 + (XX div 3);
        CoordY := YY*2 + 2 + (YY div 3) + ZZ*7*(NLigZ+1);
        Temp := Labyrinthe[CoordY];
        Temp[CoordX] := laby[XX, YY, ZZ].Car;
        if laby[XX, YY, ZZ].dir[3] then
          Temp[CoordX-1] := '0';
        if laby[XX, YY, ZZ].dir[1] then
          Temp[CoordX+1] := '0';
        Labyrinthe[CoordY] := Temp;
        if laby[XX, YY, ZZ].dir[0] then
        begin
          Temp := Labyrinthe[CoordY-1];
          Temp[CoordX] := '0';
          Labyrinthe[CoordY-1] := Temp;
        end;
        if laby[XX, YY, ZZ].dir[2] then
        begin
          Temp := Labyrinthe[CoordY+1];
          Temp[CoordX] := '0';
          Labyrinthe[CoordY+1] := Temp;
        end;
      end;

  Labyrinthe.Delete(0);
  NomLabyrinthe := FormPrincipale.EditNomFichier.Text;
  CreeSortie(Labyrinthe, fLabyrinthsDir+NomLabyrinthe+'.flp',
    NColZ*7, NLigZ*7, NEtaZ);
  Labyrinthe.Free;
end;

procedure TFormPrincipale.initMID;
begin
  EditPeriode.Value := EditLignes.Value+EditColonnes.Value+
    (EditEtages.Value div 3);
end;

procedure TFormPrincipale.BExeClick(Sender: TObject);
begin
  if EditNomFichier.Text = '' then
    EditNomFichier.Text := 'LabyGene';
  NLigZ := EditLignes.Value;
  NLig := NLigZ * 3;
  NColZ := EditColonnes.Value;
  NCol := NColZ * 3;
  NEtaZ := EditEtages.Value;
  NEta := NEtaZ;
  IIModulation := RGModulation.ItemIndex;
  PBlocage := EditBlocage.Value;
  Mid := EditPeriode.Value;
  PBlocEscaliers := EditBlocEscaliers.Value;
  bCarrefours := CBCarrefours.Checked;
  PBlocBoucles := EditBlocBoucles.Value;
  Principale;
  ShowMessage('Labyrinthe de profondeur '+
    IntToStr(laby[Xmn, Ymn, Zmn].niveau)+' créé.');
  BQuitter.Enabled := True;
  BQuitter.SetFocus;
end;

procedure TFormPrincipale.BAideClick(Sender: TObject);
begin
  Application.HelpContext(1);
end;

procedure TFormPrincipale.BQuitterClick(Sender: TObject);
begin
  if FileExists(Dir+'FunLaby.exe') then
    WinExec(PChar('"'+Dir+'FunLaby.exe" "'+
      fLabyrinthsDir+NomLabyrinthe+'.flp"'),
      SW_ShowNormal);
  Close;
end;

procedure TFormPrincipale.FormCreate(Sender: TObject);
begin
  Application.HelpFile := Dir+'GeneLaby.hlp';
end;

procedure TFormPrincipale.RGModulationClick(Sender: TObject);
begin
  if (RGModulation.ItemIndex = 1) then
  begin
    EditBlocage.Value := 80;
  end else
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

