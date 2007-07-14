{*
  Bo�te de dialogue affichant et modifiant les propri�t�s d'un joueur
  L'unit� PlayerAttributes propose une bo�te de dialogue affichant les attributs
  d'un joueur, et permettant de les modifier.
  @author sjrd
  @version 5.0
*}
unit PlayerAttributes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ValEdit, ScDelphiLanguage, FunLabyUtils;

resourcestring
  sWrongKeyFormat = 'Les noms d''attributs ne doivent �tre constitu�s que de '+
    'lettres non accentu�es et de chiffres';

type
  {*
    Bo�te de dialogue affichant et modifiant les propri�t�s d'un joueur
    @author sjrd
    @version 5.0
  *}
  TFormAttributes = class(TForm)
    LabelAttributes: TLabel;
    ValueListAttributes: TValueListEditor;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure ValueListAttributesValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
    class function ManageAttributes(Player : TPlayer) : boolean;
  end;

implementation

{$R *.dfm}

{*
  Affiche et permet de modifier les attributs d'un joueur
  @param Player   Joueur concern�
  @return True si une modification a eu lieu, False sinon
*}
class function TFormAttributes.ManageAttributes(Player : TPlayer) : boolean;
var Attributes : TStrings;
    I : integer;
begin
  with Create(Application) do
  try
    Attributes := TStringList.Create;
    try
      Player.GetAttributes(Attributes);

      for I := 0 to Attributes.Count-1 do
      begin
        ValueListAttributes.Strings.Values[Attributes[I]] :=
          IntToStr(integer(Attributes.Objects[I]));
      end;

      if ShowModal <> mrOK then Result := False else
      begin
        for I := 0 to Attributes.Count-1 do
          Player.Attribute[Attributes[I]] := 0;
        with ValueListAttributes.Strings do for I := 0 to Count-1 do
          Player.Attribute[Names[I]] := StrToIntDef(ValueFromIndex[I], 0);

        Result := True;
      end;
    finally
      Attributes.Free;
    end;
  finally
    Release;
  end;
end;

{*
  Gestionnaire d'�v�nement OnValidate de la liste de valeurs
  @param Sender     Objet qui a d�clench� l'�v�nement
  @param ACol       Index de colonne de la cellule � valider
  @param ARow       Index de ligne de la cellule � valider
  @param KeyName    Nom de la clef � valider
  @param KeyValue   Valeur de la clef � valider
*}
procedure TFormAttributes.ValueListAttributesValidate(Sender: TObject;
  ACol, ARow: Integer; const KeyName, KeyValue: string);
begin
  if not CorrectIdentifier(KeyName) then
    raise EConvertError.Create(sWrongKeyFormat);
  if KeyValue <> '' then
    StrToInt(KeyValue);
end;

end.

