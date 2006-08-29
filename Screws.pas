unit Screws;

interface

uses
  Graphics, LabyrintheUtils;

type
  TGrass = class(TScrew)
  public
    procedure Draw(Canvas : TCanvas; X, Y : integer); override;
  end;

implementation

/////////////////////
/// Classe TGrass ///
/////////////////////

procedure TGrass.Draw(Canvas : TCanvas; X, Y : integer);
begin
  inherited;
end;

end.

