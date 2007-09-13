{*
  Bo�te de dialogue affichant et modifiant les propri�t�s d'un joueur
  L'unit� PlayerAttributes propose une bo�te de dialogue affichant les attributs
  d'un joueur, et permettant de les modifier.
  @author sjrd
  @version 5.0
*}
unit EditParameters;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, ValEdit, ScDelphiLanguage, FunLabyUtils, FilesUtils;

resourcestring
  sUnitParamsCaption = 'Param�tres de l''unit�';
  sUnitParamsPrompt = 'Param�tres de l''unit� :';
  sUnitParamsKeyName = 'Nom';
  sUnitParamsValueName = 'Valeur';

  sWrongKeyFormat = 'Les noms d''attributs ne doivent �tre constitu�s que de '+
    'lettres non accentu�es et de chiffres';

type
  {*
    Bo�te de dialogue affichant et modifiant des listes de paires nom/valeur
    @author sjrd
    @version 5.0
  *}
  TFormParameters = class(TForm)
    LabelParams: TLabel;
    ValueListParams: TValueListEditor;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure ValueListParamsValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
  private
    { D�clarations priv�es }
    DataType: (dtString, dtInteger);
  public
    { D�clarations publiques }
    class function EditPlayerAttributes(Player: TPlayer): Boolean;
    class function EditUnitParams(var Params: TUnitFileParams): Boolean;
  end;

implementation

{$R *.dfm}

{*
  Affiche et permet de modifier les attributs d'un joueur
  @param Player   Joueur concern�
  @return True si une modification a eu lieu, False sinon
*}
class function TFormParameters.EditPlayerAttributes(Player: TPlayer): Boolean;
var
  Attributes: TStrings;
  I: Integer;
begin
  with Create(Application) do
  try
    DataType := dtInteger;

    Attributes := TStringList.Create;
    try
      Player.GetAttributes(Attributes);

      for I := 0 to Attributes.Count-1 do
      begin
        ValueListParams.Strings.Values[Attributes[I]] :=
          IntToStr(Integer(Attributes.Objects[I]));
      end;

      if ShowModal <> mrOk then
        Result := False
      else
      begin
        for I := 0 to Attributes.Count-1 do
          Player.Attribute[Attributes[I]] := 0;
        with ValueListParams.Strings do
          for I := 0 to Count-1 do
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
  �dite la liste des param�tres d'une unit�
  @param Params   Liste des param�tres
  @return True si la liste a �t� modifi�e, False sinon
*}
class function TFormParameters.EditUnitParams(
  var Params: TUnitFileParams): Boolean;
var
  I: Integer;
begin
  with Create(Application), ValueListParams do
  try
    DataType := dtString;
    Caption := sUnitParamsCaption;
    LabelParams.Caption := sUnitParamsPrompt;
    TitleCaptions[0] := sUnitParamsKeyName;
    TitleCaptions[1] := sUnitParamsValueName;

    for I := 0 to Length(Params)-1 do
      Strings.Values[Params[I].Name] := Params[I].Value;

    if ShowModal <> mrOk then
      Result := False
    else
    begin
      SetLength(Params, Strings.Count);
      for I := 0 to Length(Params)-1 do
      begin
        with Params[I] do
        begin
          Name := Strings.Names[I];
          Value := Strings.ValueFromIndex[I];
        end;
      end;

      Result := True;
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
procedure TFormParameters.ValueListParamsValidate(Sender: TObject;
  ACol, ARow: Integer; const KeyName, KeyValue: string);
begin
  if not CorrectIdentifier(KeyName) then
    raise EConvertError.Create(sWrongKeyFormat);

  if (DataType = dtInteger) and (KeyValue <> '') then
    StrToInt(KeyValue);
end;

end.

