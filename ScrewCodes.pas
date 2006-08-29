unit ScrewCodes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ComCtrls, ScStrUtils;

type
  TFormCodesCases = class(TForm)
    Pages: TTabControl;
    Grille: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GrilleDblClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure CasesCommunes;
    procedure Boutons;
    procedure Teleporteurs;
    procedure Locomotion;
  public
    { Déclarations publiques }
  end;

var
  FormCodesCases: TFormCodesCases;

implementation

{$R *.dfm}

uses EditActionsMain;

const Commons : array[0..24] of string =
(
  'Herbe 0', 'Eau 1', 'Mur 2', 'Trou 3', 'Bloc d''argent 4',
  'Bloc d''or 5', 'Flèche Nord 6', 'Flèche Est 7', 'Flèche Sud 8',
  'Flèche Ouest 9', 'Carrefour :', 'Tourn. Dir. à (0224)',
  'Tourn. Ind. á (0225)', 'Esc. montant >', 'Esc. descendant <',
  'Ascenseur =', 'Départ A', 'Sortie B', 'Trésor ;', 'Bouée C', 'Planche D',
  'Clé d''argent E', 'Clé d''or F', 'Passage secret ?', 'Ciel â (0226)'
);

procedure TFormCodesCases.CasesCommunes;
var I : integer;
begin
  for I := 0 to 24 do
    Grille.Cells[I mod 5, I div 5] := Commons[I];
end;

procedure TFormCodesCases.Boutons;
var I : integer;
begin
  Grille.Cells[0, 0] := 'Enfoncé @';
  for I := 1 to 15 do
    Grille.Cells[I mod 5, I div 5] := IntToStr(I)+' '+Char(32+I);
  for I := 16 to 45 do
    Grille.Cells[I mod 5, I div 5] := IntToStr(I)+' '+Char(145+I)+' (0'+IntToStr(145+I)+')';
end;

procedure TFormCodesCases.Teleporteurs;
var I : integer;
begin
  Grille.Cells[0, 0] := '0 ` (96)';
  for I := 1 to 30 do
    Grille.Cells[I mod 5, I div 5] := IntToStr(I)+' '+Char(96+I);
end;

procedure TFormCodesCases.Locomotion;
var I : integer;
begin
  Grille.Cells[0, 0] := 'Barques';
  for I := 1 to 10 do
    Grille.Cells[(I+4) mod 5, (I+4) div 5] := IntToStr(I)+' '+Char(192+I)+' (0'+IntToStr(192+I)+')';
end;

procedure TFormCodesCases.FormCreate(Sender: TObject);
begin
  CasesCommunes;
end;

procedure TFormCodesCases.PagesChange(Sender: TObject);
var I : integer;
begin
  for I := 0 to 53 do Grille.Cells[I mod 5, I div 5] := '';
  case Pages.TabIndex of
    0 : CasesCommunes;
    1 : Boutons;
    2 : Teleporteurs;
    3 : Locomotion;
  end;
end;

procedure TFormCodesCases.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FormPrincipale.VoirCodesCases.Checked := False;
end;

procedure TFormCodesCases.GrilleDblClick(Sender: TObject);
var X, Y : integer;
    Insertion : string;
begin
  if not FormPrincipale.EnTete.Enabled then exit;
  X := Grille.Selection.Left;
  Y := Grille.Selection.Top;
  if Pages.TabIndex = 0 then
  begin
    Insertion := GetXToken(Grille.Cells[X, Y], ' ', 3);
    if (Insertion = '') or (Insertion[1] = '(') then
      Insertion := GetXToken(Grille.Cells[X, Y], ' ', 2);
    FormPrincipale.EnTete.SelText := Insertion;
  end else
    FormPrincipale.EnTete.SelText := GetXToken(Grille.Cells[X, Y], ' ', 2);
end;

end.
