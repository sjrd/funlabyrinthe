{*
  G�n�ration du base de carte
  L'unit� MapBase d�finit une bo�te de dialogue cr�ant une base de carte.
  @author sjrd
  @version 5.0
*}
unit MapBase;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ScUtils, FunLabyUtils;

type
  {*
    Bo�te de dialogue cr�ant une base de carte
    @author sjrd
    @version 5.0
  *}
  TFormMapBase = class(TForm)
  private
    { D�clarations priv�es }
    Master: TMaster; /// Ma�tre FunLabyrinthe
    Map: TMap;       /// Carte � cr�er

    procedure GenerateBasicMap;
  public
    { D�clarations publiques }
    class procedure GenerateBase(AMap: TMap);
  end;

var
  FormMapBase: TFormMapBase;

implementation

{$R *.dfm}

const
  idOutsideSquare = 'Outside---'; /// ID de la case d'ext�rieur
  idWallSquare = 'Wall---';       /// ID de la case de mur
  idGrassSquare = 'Grass---';     /// ID de la case d'herbe

{*
  G�n�re la carte basique faite d'herbe et d'une ceinture de murs
*}
procedure TFormMapBase.GenerateBasicMap;
var
  X, Y, Z: Integer;
begin
  for Z := 0 to Map.Dimensions.Z-1 do
  begin
    Map.Outside[Z] := Master.Square[idOutsideSquare];

    for X := 0 to Map.Dimensions.X-1 do
    begin
      for Y := 0 to Map.Dimensions.Y-1 do
      begin
        if (X = 0) or (X = Map.Dimensions.X-1) or
          (Y = 0) or (Y = Map.Dimensions.Y -1) then
          Map[Point3D(X, Y, Z)] := Master.Square[idWallSquare]
        else
          Map[Point3D(X, Y, Z)] := Master.Square[idGrassSquare];
      end;
    end;
  end;
end;

{*
  G�n�re la carte sp�cifi�e
  @param AMap   La carte � g�n�rer
*}
class procedure TFormMapBase.GenerateBase(AMap: TMap);
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

