{*
  Bo�te de dialogue affichant les objets poss�d�s par un joueur
  L'unit� PlayerObjects propose une bo�te de dialogue affichant les objets
  poss�d�s par un joueur.
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
    Bo�te de dialogue affichant les objets poss�d�s par un joueur
    @author sjrd
    @version 5.0
  *}
  TFormObjects = class(TForm)
    LabelObjects: TLabel;
    ListViewObjects: TListView;
    ButtonOK: TButton;
    ObjectsImages: TImageList;
  private
    { D�clarations prives }
  public
    { D�clarations publiques }
    class procedure ShowObjects(Player: TPlayer);
  end;

implementation

{$R *.dfm}

{*
  Affiche les objets d'un joueur
  @param Player   Joueur concern�
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

