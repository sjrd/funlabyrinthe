unit LabMap;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, EditActionsMain, LabyrintheUtils, StdCtrls,
  Spin, ExtCtrls, ScUtils;

type
  TFormPlan = class(TForm)
    Horizontal: TScrollBar;
    Image: TImage;
    Vertical: TScrollBar;
    Etage: TSpinButton;
    EditEtage: TSpinEdit;
    LabelEtage: TLabel;
    GroupInfos: TGroupBox;
    LabelCase: TLabel;
    LabelCode: TLabel;
    LabelZone: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HorizontalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure VerticalScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure EditEtageChange(Sender: TObject);
    procedure EtageDownClick(Sender: TObject);
    procedure EtageUpClick(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
    Cols, Rows, Etas : integer;
    LaCase : T3DPoint;
    //Cases : TBitmap;
    SelectX, SelectY : integer;
    function GetEcran(X, Y : integer) : integer;
  public
    { Déclarations publiques }
    Form : TFormPrincipale;
    procedure Ouvre;
    procedure Affiche;
    property Ecran[X, Y : integer] : integer read GetEcran;
  end;

var
  FormPlan : TFormPlan;

function GetRect(X, Y : integer) : TRect;
function Montant(X, Y, Z : integer) : boolean;
function IntToStr0(I, Longueur : integer) : string;

procedure ContourRectangle(Canvas : TCanvas; X1, Y1, X2, Y2 : integer);

implementation

{$R *.DFM}

procedure ContourRectangle(Canvas : TCanvas; X1, Y1, X2, Y2 : integer);
begin
  with Canvas do
  begin
    MoveTo(X1, Y1);
    LineTo(X2, Y1);
    LineTo(X2, Y2);
    LineTo(X1, Y2);
    LineTo(X1, Y1);
  end;
end;

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

function IntToStr0(I, Longueur : integer) : string;
begin
  Result := IntToStr(I);
  while Length(Result) < Longueur do Result := '0'+Result;
end;

function TFormPlan.GetEcran(X, Y : integer) : integer;
begin
  Result := Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z];
  if Result in Escalier then
  begin
    if Montant(LaCase.X+X, LaCase.Y+Y, LaCase.Z)
    then Result := 1
    else Result := 2;
  end;
end;

procedure TFormPlan.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Form.VoirPlanLaby.Checked := False;
end;

procedure TFormPlan.Ouvre;
begin
  Cols := Labyrinthe.Dimensions.Colonnes;
  Rows := Labyrinthe.Dimensions.Lignes;
  Etas := Labyrinthe.Dimensions.Etages;
  Horizontal.Max := (Cols-1) * 7;
  Vertical.Max   := (Rows-1) * 7;
  EditEtage.MaxValue := Etas;
  Horizontal.Enabled := (Horizontal.Max > 0);
  Vertical  .Enabled := (Vertical  .Max > 0);
  EditEtage .Enabled := (EditEtage.MaxValue > 1);
  Etage.Enabled := EditEtage.Enabled;
  LaCase.X := 0;
  LaCase.Y := 0;
  LaCase.Z := 1;
  SelectX := 3;
  SelectY := 3;
  Affiche;
end;

procedure TFormPlan.Affiche;
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
        if (N <= 127) then dec(N, 32) else dec(N, 145);
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
    Brush.Color := clYellow;
    Pen.Color := clYellow;
  end;
  X := SelectX;
  Y := SelectY;
  ContourRectangle(Img.Canvas, X*30-1, Y*30-1, X*30+31, Y*30+31);
  ContourRectangle(Img.Canvas, X*30  , Y*30  , X*30+30, Y*30+30);
  ContourRectangle(Img.Canvas, X*30+1, Y*30+1, X*30+29, Y*30+29);
  Image.Picture.Bitmap.Assign(Img);
  Img.Free;
  EditEtage.Value := LaCase.Z;
  LabelCase.Caption := 'Case :  '+IntToStr(LaCase.X+X)+'  '+
                       IntToStr(LaCase.Y+Y)+'  '+IntToStr(LaCase.Z);
  LabelZone.Caption := 'Zone :  '+IntToStr((LaCase.X+X) div 7)+'  '+
                       IntToStr((LaCase.Y+Y) div 7)+'  '+IntToStr(LaCase.Z);
  N := Labyrinthe[LaCase.X+X, LaCase.Y+Y, LaCase.Z];
  if N = 38 then LabelCode.Caption := 'Code :  &&'
            else LabelCode.Caption := 'Code :  '+ Char(N);
  if (N >= 128) then LabelCode.Caption := LabelCode.Caption+' (0'+IntToStr(N)+')';
end;

procedure TFormPlan.FormCreate(Sender: TObject);
var B : TBitmap;
begin
  //Cases := TBitmap.Create;
  //Cases.LoadFromFile(Dir+'Cases\Cases.bmp');
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
end;

procedure TFormPlan.FormDestroy(Sender: TObject);
begin
  //Cases.Free;
end;

procedure TFormPlan.HorizontalScroll(Sender: TObject;
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

procedure TFormPlan.VerticalScroll(Sender: TObject;
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

procedure TFormPlan.EditEtageChange(Sender: TObject);
begin
  LaCase.Z := EditEtage.Value;
  Affiche;
end;

procedure TFormPlan.EtageDownClick(Sender: TObject);
begin
  if LaCase.Z > 1 then
  begin
    dec(LaCase.Z);
    Affiche;
  end;
end;

procedure TFormPlan.EtageUpClick(Sender: TObject);
begin
  if LaCase.Z < Labyrinthe.Dimensions.Etages then
  begin
    inc(LaCase.Z);
    Affiche;
  end;
end;

procedure TFormPlan.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SelectX := X div 30;
  SelectY := Y div 30;
  Affiche;
end;

end.
