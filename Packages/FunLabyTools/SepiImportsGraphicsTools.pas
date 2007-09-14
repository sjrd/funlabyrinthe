{*
  Importe l'unité GraphicsTools dans un environnement Sepi
  @author sjrd
  @version 1.0
*}
unit SepiImportsGraphicsTools;

interface

uses
  TypInfo, SepiReflectionCore, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiMembers, GraphicsTools;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root: TSepiRoot): TSepiUnit;
begin
  Result := TSepiUnit.Create(Root, 'GraphicsTools',
    ['Graphics']);

  // Routines
  TSepiMethod.Create(Result, 'DrawScrewNumber', @DrawScrewNumber,
    'procedure(Canvas : TCanvas; X, Y, Number : integer; FontColor : TColor = clBlack )');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('GraphicsTools', ImportUnit);
end.

