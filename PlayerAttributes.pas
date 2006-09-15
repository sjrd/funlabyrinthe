unit PlayerAttributes;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ValEdit, ScExtra, FunLabyUtils;

resourcestring
  sWrongKeyFormat = 'Les noms d''attributs ne doivent être constitués que de'+
    'lettres non accentuées et de chiffres';

type
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

var
  FormAttributes: TFormAttributes;

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

procedure TFormAttributes.ValueListAttributesValidate(Sender: TObject;
  ACol, ARow: Integer; const KeyName, KeyValue: string);
begin
  if not CorrectIdentifier(KeyName) then
    raise EConvertError.Create(sWrongKeyFormat);
  if KeyValue <> '' then
    StrToInt(KeyValue);
end;

end.

