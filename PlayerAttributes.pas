{*
  Boîte de dialogue affichant et modifiant les propriétés d'un joueur
  L'unité PlayerAttributes propose une boîte de dialogue affichant les attributs
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
  sWrongKeyFormat = 'Les noms d''attributs ne doivent être constitués que de '+
    'lettres non accentuées et de chiffres';

type
  {*
    Boîte de dialogue affichant et modifiant les propriétés d'un joueur
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
    { Déclarations privées }
  public
    { Déclarations publiques }
    class function ManageAttributes(Player : TPlayer) : boolean;
  end;

implementation

{$R *.dfm}

{*
  Affiche et permet de modifier les attributs d'un joueur
  @param Player   Joueur concerné
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
  Gestionnaire d'événement OnValidate de la liste de valeurs
  @param Sender     Objet qui a déclenché l'événement
  @param ACol       Index de colonne de la cellule à valider
  @param ARow       Index de ligne de la cellule à valider
  @param KeyName    Nom de la clef à valider
  @param KeyValue   Valeur de la clef à valider
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

