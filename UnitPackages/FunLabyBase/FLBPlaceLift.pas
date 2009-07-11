unit FLBPlaceLift;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ScUtils, FunLabyUtils, MapTools;

type
  {*
    Boîte de dialogue de placement d'un ascenseur
    @author sjrd
    @version 5.0
  *}
  TFormPlaceLift = class(TForm)
    ListBoxFloorList: TListBox;
    LabelFloorList: TLabel;
    ButtonOK: TBitBtn;
    ButtonCancel: TBitBtn;
  public
    class function PlaceLift(const QPos: TQualifiedPos; Lift: TEffect): Boolean;
  end;

implementation

{$R *.dfm}

{*
  Place un ascenseur
  @param QPos   Positon de départ
  @param Lift   Effet ascenseur à placer
  @return True si un ascenseur a été placé, False sinon
*}
class function TFormPlaceLift.PlaceLift(const QPos: TQualifiedPos;
  Lift: TEffect): Boolean;
var
  Map: TMap;
  Pos: T3DPoint;
  FloorCount, Floor: Integer;
begin
  with Create(Application) do
  try
    Map := QPos.Map;
    Pos := QPos.Position;
    FloorCount := Map.Dimensions.Z;

    for Floor := FloorCount-1 downto 0 do
    begin
      Pos.Z := Floor;
      ListBoxFloorList.Items.Add(Format('%d - %s', [Floor, Map[Pos].Name]));
    end;

    ListBoxFloorList.Selected[FloorCount-1-QPos.Position.Z] := True;

    Result := ShowModal = mrOK;

    if Result then
    begin
      for Floor := 0 to FloorCount-1 do
      begin
        Pos.Z := Floor;
        if ListBoxFloorList.Selected[FloorCount-1-Floor] then
          Map[Pos] := ChangeEffect(Map[Pos], Lift);
      end;
    end;
  finally
    Release;
  end;
end;

end.

