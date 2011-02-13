{*
  Constantes et classes communes
  L'unité C4xCommon décrit les constantes qui sont communes aux autres parties.
  @author sjrd
  @version 5.0
*}
unit C4xCommon;

interface

const
  phPushing = 1; /// Phase "pushing"
  phExecute = 2; /// Phase "execute"

  MinActionsCount = 78; /// Nombre minimal d'actions
  MaxVar = 20;          /// Indice maximal de variable

const {don't localize}
  idGrassSquare = 'Grass---'; /// ID de la case herbe
  idWaterSquare = 'Water---'; /// ID de la case eau

  DefaultSquaresImgName = 'Cases'; /// Fichier Cases par défaut

resourcestring
  SCategoryButtons = 'Boutons'; /// Catégorie des boutons

const {don't localize}
  fCompatibility4xPlayer = 'Compatibility4x/Player';
  fCompatibility4xBuoyPlugin = 'Compatibility4x/BuoyPlugin';

function RemoveDiacritics(const Value: string): string;

implementation

uses
  Windows;

{*
  Retire les diacritiques dans une chaîne de caractères
  @param Value   Chaîne dont retirer les diacritiques
  @return Value dont les diacritiques ont été retirées
*}
function RemoveDiacritics(const Value: string): string;
var
  FoldedValue: string;
  I, Len: Integer;
begin
  // Fold the string to separate diacritics from their letter
  Len := FoldString(MAP_COMPOSITE, PChar(Value), -1, nil, 0) - 1;
  SetLength(FoldedValue, Len);
  FoldString(MAP_COMPOSITE, PChar(Value), -1, PChar(FoldedValue), Len+1);

  // Keep only US-ASCII characters
  SetLength(Result, Len);
  Len := 0;

  for I := 1 to Length(FoldedValue) do
  begin
    if Integer(FoldedValue[I]) <= $7F then
    begin
      Inc(Len);
      Result[Len] := FoldedValue[I];
    end;
  end;

  SetLength(Result, Len);
end;

end.

