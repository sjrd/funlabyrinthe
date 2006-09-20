unit MapBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ScUtils, FunLabyUtils;

type
  TFormMapBase = class(TForm)
  private
    { Déclarations privées }
    Master : TMaster;
    Map : TMap;

    procedure GenerateBasicMap;
  public
    { Déclarations publiques }
    class procedure GenerateBase(AMap : TMap);
  end;

var
  FormMapBase: TFormMapBase;

implementation

{$R *.dfm}

const
  idOutsideScrew = 'Grass-Outside-';
  idWallScrew = 'Wall--';
  idGrassScrew = 'Grass--';

{*
  Génère la carte basique faite d'herbe et d'une ceinture de murs
*}
procedure TFormMapBase.GenerateBasicMap;
var X, Y, Z : integer;
begin
  for Z := 0 to Map.Dimensions.Z-1 do
  begin
    Map.Outside[Z] := Master.Screw[idOutsideScrew];

    for X := 0 to Map.Dimensions.X-1 do for Y := 0 to Map.Dimensions.Y-1 do
    begin
      if (X = 0) or (X = Map.Dimensions.X-1) or
         (Y = 0) or (Y = Map.Dimensions.Y -1) then
        Map[Point3D(X, Y, Z)] := Master.Screw[idWallScrew]
      else
        Map[Point3D(X, Y, Z)] := Master.Screw[idGrassScrew];
    end;
  end;
end;

{*
  Génère la carte spécifiée
  @param AMap   La carte à générer
*}
class procedure TFormMapBase.GenerateBase(AMap : TMap);
begin
  with Create(Application) do
  try
    Master := AMap.Master;
    Map := AMap;

    GenerateBasicMap;
  finally
    Release;
  end;
end;

end.

