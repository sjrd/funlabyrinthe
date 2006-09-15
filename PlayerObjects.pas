unit PlayerObjects;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, FunLabyUtils;

type
  TFormObjects = class(TForm)
    LabelObjects: TLabel;
    ListViewObjects: TListView;
    ButtonOK: TButton;
    ObjectsImages: TImageList;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    class procedure ShowObjects(Player : TPlayer);
  end;

var
  FormObjects: TFormObjects;

implementation

{$R *.dfm}

{*
  Affiche les objets d'un joueur
  @param Player   Joueur concerné
*}
class procedure TFormObjects.ShowObjects(Player : TPlayer);
var Bitmap : TBitmap;
    Master : TMaster;
    I : integer;
    Infos : string;
begin
  with Create(Application) do
  try
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := ScrewSize;
      Bitmap.Height := ScrewSize;

      Master := Player.Master;

      for I := 0 to Master.ObjectDefCount-1 do with Master.ObjectDefs[I] do
      begin
        Infos := ShownInfos[Player];
        if Infos <> '' then
        begin
          EmptyScrewRect(Bitmap.Canvas);
          Draw(Bitmap.Canvas);

          with ListViewObjects.Items.Add do
          begin
            Caption := Infos;
            ImageIndex := ObjectsImages.AddMasked(Bitmap, clTransparent);
          end;
        end;
      end;
    finally
      Bitmap.Free;
    end;

    ShowModal;
  finally
    Release;
  end;
end;

end.

