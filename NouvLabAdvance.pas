unit NouvLabAdvance;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, ScUtils;

type
  TParamsZone = record
    Terrain1, Terrain2, Terrain3 : integer;
  end;

  TFormParamsLabMax = class(TForm)
    BoutonOK: TBitBtn;
    BoutonAnnuler: TBitBtn;
    PlanEtage: TImage;
    LabelTerrain1: TLabel;
    LabelTerrain2: TLabel;
    ComboTerrain1: TComboBox;
    ComboTerrain2: TComboBox;
    BoutonAide: TBitBtn;
    LabelTerrain3: TLabel;
    ComboTerrain3: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboTerrain1Change(Sender: TObject);
    procedure ComboTerrain2Change(Sender: TObject);
    procedure ComboTerrain3Change(Sender: TObject);
    procedure PlanEtageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
    ParamsZones : array[0..9, 0..9] of TParamsZone;
    CasesSelects : TStringList;
    Cols, Rows : integer;
    Canvas : TCanvas;
    function Egaux4ParamsZone(Param1, Param2, Param3, Param4 : TParamsZone) : boolean;
    procedure Affiche;
    procedure ConstruitEtage;
  public
    { Déclarations publiques }
    Etage : TStringList;
    function QueryParams(ACols, ARows : integer) : boolean;
  end;

var
  FormParamsLabMax: TFormParamsLabMax;

procedure NewZoneParams(var Zone : TParamsZone);
function CheckBoxState_Egal_Boolean(Param1 : TCheckBoxState; Param2 : boolean) : boolean;
function Egaux2ParamsZone(Param1, Param2 : TParamsZone) : boolean;
function Terrain1ToChar(Terrain : integer) : Char;
function Terrain2ToChar(Terrain : integer) : Char;
function Terrain3ToChar(Terrain : integer) : Char;
function Terrain4ToChar(Terrain : integer) : Char;

implementation

{$R *.DFM}

uses ScStrUtils;

function Terrain1ToChar(Terrain : integer) : Char;
begin
  case Terrain of
    0 : Result := '0';
    1 : Result := '1';
    2 : Result := '3';
    3 : Result := '2';
    4 : Result := 'B';
    else Result := #0;
  end;
end;

function Terrain2ToChar(Terrain : integer) : Char;
begin
  case Terrain of
    0 : Result := #0;
    1 : Result := '2';
    2 : Result := '1';
    3 : Result := '0';
    4 : Result := '3';
    5 : Result := 'B';
    else Result := #0;
  end;
end;

function Terrain3ToChar(Terrain : integer) : Char;
begin
  case Terrain of
    0 : Result := #0;
    1 : Result := '2';
    2 : Result := '1';
    3 : Result := '0';
    4 : Result := '3';
    5 : Result := 'B';
    else Result := #0;
  end;
end;

function Terrain4ToChar(Terrain : integer) : Char;
begin
  case Terrain of
    0 : Result := 'B';
    1 : Result := '1';
    2 : Result := '0';
    3 : Result := '2';
    4 : Result := 'â';
    5 : Result := '3';
    else Result := #0;
  end;
end;

procedure NewZoneParams(var Zone : TParamsZone);
begin
  Zone.Terrain1 := 0;
  Zone.Terrain2 := 0;
  Zone.Terrain3 := 1;
end;

function CheckBoxState_Egal_Boolean(Param1 : TCheckBoxState; Param2 : boolean) : boolean;
begin
  case Param1 of
    cbChecked   : Result := Param2;
    cbUnchecked : Result := (not Param2);
    cbGrayed    : Result := False;
    else Result := False;
  end;
end;

function Egaux2ParamsZone(Param1, Param2 : TParamsZone) : boolean;
begin
  Result := (Param1.Terrain1 = Param2.Terrain1) and
            (Param1.Terrain2 = Param2.Terrain2) and
            (Param1.Terrain3 = Param2.Terrain3);
end;

function TFormParamsLabMax.QueryParams(ACols, ARows : integer) : boolean;
var I, J : integer;
    B : TBitmap;
begin
  Cols := ACols;
  Rows := ARows;
  PlanEtage.Width  := Cols*21;
  PlanEtage.Height := Rows*21;
  PlanEtage.Left   := 105 - (Cols*21 div 2);
  PlanEtage.Top    := 105 - (Rows*21 div 2);
  B := TBitmap.Create;
  B.Width := Cols*21;
  B.Height := Rows*21;
  PlanEtage.Picture.Bitmap.Assign(B);
  B.Free;
  Canvas := PlanEtage.Picture.Bitmap.Canvas;
  for I := 0 to 9 do for J := 0 to 9 do NewZoneParams(ParamsZones[I, J]);
  CasesSelects.Clear;
  CasesSelects.Add('0;0');
  Affiche;
  Result := ShowModal = mrOK;
end;

function TFormParamsLabMax.Egaux4ParamsZone(Param1, Param2, Param3, Param4 : TParamsZone) : boolean;
begin
  Result := Egaux2ParamsZone(Param1, Param2) and
            Egaux2ParamsZone(Param1, Param3) and
            Egaux2ParamsZone(Param1, Param4);
end;

procedure TFormParamsLabMax.Affiche;
var I, X, Y, Terrain1, Terrain2, Terrain3 : integer;
    Color : TColor;
begin
{-----Affichage des paramètres-----}
  X := StrToInt(GetFirstToken(CasesSelects[0], ';'));
  Y := StrToInt(GetLastToken (CasesSelects[0], ';'));
  Terrain1 := ParamsZones[X, Y].Terrain1;
  Terrain2 := ParamsZones[X, Y].Terrain2;
  Terrain3 := ParamsZones[X, Y].Terrain3;
  for I := 1 to CasesSelects.Count -1 do
  begin
    X := StrToInt(GetFirstToken(CasesSelects[I], ';'));
    Y := StrToInt(GetLastToken (CasesSelects[I], ';'));
    if (Terrain1 <> -1) and (ParamsZones[X, Y].Terrain1 <> Terrain1) then
      Terrain1 := -1;
    if (Terrain2 <> -1) and (ParamsZones[X, Y].Terrain2 <> Terrain2) then
      Terrain2 := -1;
    if (Terrain3 <> -1) and (ParamsZones[X, Y].Terrain3 <> Terrain3) then
      Terrain3 := -1;
  end;
  ComboTerrain1.ItemIndex := Terrain1;
  ComboTerrain2.ItemIndex := Terrain2;
  ComboTerrain3.ItemIndex := Terrain3;
{-----Affichage du labyrinthe-----}
  ConstruitEtage;
  Canvas.Pen.Width := 1;
  for X := 0 to Cols*7-1 do for Y := 0 to Rows*7-1 do
  begin
    case Etage[Y][X+1] of
      '0' : Color := clGreen;
      '1' : Color := clAqua;
      '2' : Color := clRed;
      '3' : Color := clBlack;
      'B' : Color := clWhite;
      else Color := clBlack;
    end;
    with Canvas do
    begin
      Brush.Color := Color;
      Pen  .Color := Color;
      Canvas.Rectangle(X*3, Y*3, X*3+3, Y*3+3);
    end;
  end;
  with Canvas do
  begin
    Pen.Color := clYellow;
    Pen.Width := 2;
    for X := 0 to Cols-1 do for Y := 0 to Rows-1 do
    if CasesSelects.IndexOf(IntToStr(X)+';'+IntToStr(Y)) <> -1 then
    begin
      MoveTo(X*21   , Y*21   );
      LineTo(X*21+21, Y*21   );
      LineTo(X*21+21, Y*21+21);
      LineTo(X*21   , Y*21+21);
      LineTo(X*21   , Y*21   );
    end;
  end;
end;

procedure TFormParamsLabMax.ConstruitEtage;
var I, J, X, Y : integer;
    Terrain : Char;
    Str : string;
    Eta : array[0..69, 0..69] of Char;
begin
  for I := 0 to Cols-1 do for J := 0 to Rows-1 do
  begin
    Terrain := Terrain1ToChar(ParamsZones[I, J].Terrain1);
    for X := 0 to 6 do for Y := 0 to 6 do
      Eta[I*7+X, J*7+Y] := Terrain;
  end;
  for I := 0 to Cols-1 do for J := 0 to Rows-1 do
  begin
    Terrain := Terrain2ToChar(ParamsZones[I, J].Terrain2);
    if Terrain = #0 then Continue;
    for X := 0 to 6 do for Y := 0 to 6 do
      if (X mod 2 = 0) and (Y mod 2 = 0) then
        Eta[I*7+X, J*7+Y] := Terrain;
  end;
  for I := 0 to Cols-1 do for J := 0 to Rows-1 do
  begin
    Terrain := Terrain3ToChar(ParamsZones[I, J].Terrain3);
    if Terrain = #0 then Continue;
    if (I = 0) or (not Egaux2ParamsZone(ParamsZones[I, J], ParamsZones[I-1, J])) then
      for Y := 1 to 5 do Eta[I*7, J*7+Y] := Terrain;
    if (J = 0) or (not Egaux2ParamsZone(ParamsZones[I, J], ParamsZones[I, J-1])) then
      for X := 1 to 5 do Eta[I*7+X, J*7] := Terrain;
    if (I = Cols-1) or (not Egaux2ParamsZone(ParamsZones[I, J], ParamsZones[I+1, J])) then
      for Y := 1 to 5 do Eta[I*7+6, J*7+Y] := Terrain;
    if (J = Rows-1) or (not Egaux2ParamsZone(ParamsZones[I, J], ParamsZones[I, J+1])) then
      for X := 1 to 5 do Eta[I*7+X, J*7+6] := Terrain;
    if (I = 0) or (J = 0) or (not Egaux4ParamsZone(
       ParamsZones[I, J], ParamsZones[I-1, J],
       ParamsZones[I, J-1], ParamsZones[I-1, J-1])) then
         Eta[I*7, J*7] := Terrain;
    if (I = 0) or (J = Rows-1) or (not Egaux4ParamsZone(
       ParamsZones[I, J], ParamsZones[I-1, J],
       ParamsZones[I, J+1], ParamsZones[I-1, J+1])) then
         Eta[I*7, J*7+6] := Terrain;
    if (I = Cols-1) or (J = 0) or (not Egaux4ParamsZone(
       ParamsZones[I, J], ParamsZones[I+1, J],
       ParamsZones[I, J-1], ParamsZones[I+1, J-1])) then
         Eta[I*7+6, J*7] := Terrain;
    if (I = Cols-1) or (J = Rows-1) or (not Egaux4ParamsZone(
       ParamsZones[I, J], ParamsZones[I+1, J],
       ParamsZones[I, J+1], ParamsZones[I+1, J+1])) then
         Eta[I*7+6, J*7+6] := Terrain;
  end;
  Etage.Clear;
  for Y := 0 to Rows*7-1 do
  begin
    Str := '';
    for X := 0 to Cols*7-1 do Str := Str+Eta[X, Y];
    Etage.Add(Str);
  end;
end;

procedure TFormParamsLabMax.FormCreate(Sender: TObject);
begin
  Etage        := TStringList.Create;
  CasesSelects := TStringList.Create;
end;

procedure TFormParamsLabMax.FormDestroy(Sender: TObject);
begin
  Etage       .Free;
  CasesSelects.Free;
end;

procedure TFormParamsLabMax.ComboTerrain1Change(Sender: TObject);
var I, X, Y : integer;
begin
  for I := 0 to CasesSelects.Count-1 do
  begin
    X := StrToInt(GetFirstToken(CasesSelects[I], ';'));
    Y := StrToInt(GetLastToken (CasesSelects[I], ';'));
    ParamsZones[X, Y].Terrain1 := ComboTerrain1.ItemIndex;
  end;
  Affiche;
end;

procedure TFormParamsLabMax.ComboTerrain2Change(Sender: TObject);
var I, X, Y : integer;
begin
  for I := 0 to CasesSelects.Count-1 do
  begin
    X := StrToInt(GetFirstToken(CasesSelects[I], ';'));
    Y := StrToInt(GetLastToken (CasesSelects[I], ';'));
    ParamsZones[X, Y].Terrain2 := ComboTerrain2.ItemIndex;
  end;
  Affiche;
end;

procedure TFormParamsLabMax.ComboTerrain3Change(Sender: TObject);
var I, X, Y : integer;
begin
  for I := 0 to CasesSelects.Count-1 do
  begin
    X := StrToInt(GetFirstToken(CasesSelects[I], ';'));
    Y := StrToInt(GetLastToken (CasesSelects[I], ';'));
    ParamsZones[X, Y].Terrain3 := ComboTerrain3.ItemIndex;
  end;
  Affiche;
end;

procedure TFormParamsLabMax.PlanEtageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var X_1, X_2, Y_1, Y_2, X1, X2, Y1, Y2 : integer;
begin
  if not (ssLeft in Shift) then exit;
  X_1 := StrToInt(GetFirstToken(CasesSelects[CasesSelects.Count-1], ';'));
  Y_1 := StrToInt(GetLastToken (CasesSelects[CasesSelects.Count-1], ';'));
  X_2 := X div 21;
  Y_2 := Y div 21;
  if ssShift in Shift then
  begin
    X1 := IIF(X_1 <= X_2, X_1, X_2);
    X2 := IIF(X_1 >  X_2, X_1, X_2);
    Y1 := IIF(Y_1 <= Y_2, Y_1, Y_2);
    Y2 := IIF(Y_1 >  Y_2, Y_1, Y_2);
    for X := X1 to X2 do for Y := Y1 to Y2 do
      if ((X <> X_1) or (Y <> Y_1)) and ((X <> X_2) or (Y <> Y_2)) then
        CasesSelects.Add(IntToStr(X)+';'+IntToStr(Y));
    CasesSelects.Add(IntToStr(X_2)+';'+IntToStr(Y_2));
  end else
  begin
    if not (ssCtrl in Shift) then CasesSelects.Clear;
    CasesSelects.Add(IntToStr(X_2)+';'+IntToStr(Y_2));
  end;
  Affiche;
end;

end.
