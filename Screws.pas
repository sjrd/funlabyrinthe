unit Screws;

interface

uses
  Graphics, ScUtils, FunLabyUtils;

resourcestring
  sGrass = 'Herbe';

const {don't localize}
  fGrass = 'Grass';

type
  TGrass = class(TScrew)
  public
    constructor Create(AMaster : TMaster; const AName : string = '';
      ACode : Char = #0);
  end;

implementation

/////////////////////
/// Classe TGrass ///
/////////////////////

constructor TGrass.Create(AMaster : TMaster; const AName : string = '';
  ACode : Char = #0);
begin
  inherited Create(AMaster, IIF(AName = '', sGrass, AName),
    IIF(ACode = #0, '0', ACode));
  Painter.ImgNames.Add(fGrass);
end;

end.

