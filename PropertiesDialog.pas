unit PropertiesDialog;

interface

uses
  LabyrintheUtils, LabyrintheMain, Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls;

type
  TFormProprietes = class(TForm)
    Objets: TListBox;
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    procedure AfficheJoueur;
    procedure AfficheLabyrinthe;
  end;

var
  FormProprietes: TFormProprietes;

implementation

{$R *.DFM}

procedure TFormProprietes.AfficheJoueur;
var I : integer;
begin
  with Objets.Items do
  begin
    Caption := 'Propri�t�s du joueur';
    Clear;
    Add('Objets poss�d�s par le joueur :');
    Add('');
    Add('Bou�e : '        +IntToStr(Bouees));
    Add('Planche : '      +IntToStr(Planches));
    Add('Cl� d''argent : '+IntToStr(ClesArgent));
    Add('Cl� d''or : '    +IntToStr(ClesOr));
    for I := 1 to 45 do with Labyrinthe.Boutons[I] do
      if Style = sObjet then Add(Nom+' : '+IntToStr(Compteur));
    ShowModal;
  end;
end;

procedure TFormProprietes.AfficheLabyrinthe;
var X, Y, Z,
    NBouees, NPlanches, NClesOr, NClesArgent,
    NBlocsOr, NBlocsArgent, NPassagesSecrets : integer;

begin
  NBouees := 0; NPlanches := 0; NClesOr := 0; NClesArgent := 0;
  NBlocsOr := 0; NBlocsArgent := 0; NPassagesSecrets := 0;
  for X := 0 to Labyrinthe.Dimensions.Colonnes*7 - 1 do
    for Y := 0 to Labyrinthe.Dimensions.Lignes*7 - 1 do
      for Z := 1 to Labyrinthe.Dimensions.Etages do
        case Labyrinthe[X, Y, Z] of
          Bouee : inc (NBouees);
          Planche : inc (NPlanches);
          CleOr : inc (NClesOr);
          CleArgent : inc (NClesArgent);
          BlocOr : inc (NBlocsOr);
          BlocArgent : inc (NBlocsArgent);
          FauxMur : inc (NPassagesSecrets);
        end;

  with Objets.Items do
  begin
    Caption := 'Propri�t�s du labyrinthe';
    Clear;
    Add('Objets usuels dans le labyrinthe :');
    Add('');
    Add('Bou�e : '+IntToStr(NBouees));
    Add('Planche : '+IntToStr(NPlanches));
    Add('Cl� d''argent : '+IntToStr(NClesArgent));
    Add('Cl� d''or : '+IntToStr(NClesOr));
    Add('');
    Add('Obstacles usuels dans le labyrinthe :');
    Add('');
    Add('Bloc argent : '+IntToStr(NBlocsArgent));
    Add('Bloc or : '+IntToStr(NBlocsOr));
    Add('Passage secret : '+IntToStr(NPassagesSecrets));
    ShowModal;
  end;
end;

end.
