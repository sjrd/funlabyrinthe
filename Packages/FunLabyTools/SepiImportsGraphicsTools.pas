{*
  Importe l'unité GraphicsTools dans un environnement Sepi
  @author Sébastien Jean Robert Doeraene
  @version 1.0
*}
unit SepiImportsGraphicsTools;

interface

uses
  TypInfo, SepiMetaUnits, SepiOrdTypes, SepiStrTypes, SepiArrayTypes,
  SepiCompTypes, GraphicsTools;

implementation

{ You must not localize any of the strings this unit contains! }

{-------------}
{ Unit import }
{-------------}

function ImportUnit(Root : TSepiMetaRoot) : TSepiMetaUnit;
begin
  Result := TSepiMetaUnit.Create(Root, 'GraphicsTools',
    ['Graphics']);

  // Routines
  TSepiMetaMethod.Create(Result, 'DrawScrewNumber', @DrawScrewNumber,
    'procedure(Canvas : TCanvas; X, Y, Number : integer; FontColor : TColor = clBlack )');

  Result.Complete;
end;

initialization
  SepiRegisterImportedUnit('GraphicsTools', ImportUnit);
end.

