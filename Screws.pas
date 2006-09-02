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
    constructor Create(AMaster : TMaster; const AName : string = '');
  end;

implementation

/////////////////////
/// Classe TGrass ///
/////////////////////

constructor TGrass.Create(AMaster : TMaster; const AName : string = '');
begin
  inherited Create(AMaster, IIF(AName = '', sGrass, AName));
  Painter.ImgNames.Add(fGrass);
end;

end.

