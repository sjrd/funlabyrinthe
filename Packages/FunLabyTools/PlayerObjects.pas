{*
  Boîte de dialogue affichant les objets possédés par un joueur
  L'unité PlayerObjects propose une boîte de dialogue affichant les objets
  possédés par un joueur.
  @author sjrd
  @version 5.0
*}
unit PlayerObjects;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, FunLabyUtils;

type
  {*
    Boîte de dialogue affichant les objets possédés par un joueur
    @author sjrd
    @version 5.0
  *}
  TFormObjects = class(TForm)
    LabelObjects: TLabel;
    ListViewObjects: TListView;
    ButtonOK: TButton;
    ObjectsImages: TImageList;
  private
    { Déclarations prives }
  public
    { Déclarations publiques }
    class procedure ShowObjects(Player: TPlayer);
  end;

implementation

{$R *.dfm}

{*
  Affiche les objets d'un joueur
  @param Player   Joueur concerné
*}
class procedure TFormObjects.ShowObjects(Player: TPlayer);
var
  Bitmap: TBitmap;
  Master: TMaster;
  I: Integer;
  Infos: string;
begin
  with Create(Application) do
  try
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := SquareSize;
      Bitmap.Height := SquareSize;

      Master := Player.Master;

      for I := 0 to Master.ObjectDefCount-1 do
      begin
        with Master.ObjectDefs[I] do
        begin
          Infos := ShownInfos[Player];
          if Infos <> '' then
          begin
            EmptySquareRect(Bitmap.Canvas);
            Draw(NoQPos, Bitmap.Canvas);

            with ListViewObjects.Items.Add do
            begin
              Caption := Infos;
              ImageIndex := ObjectsImages.AddMasked(Bitmap, clTransparent);
            end;
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

