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
  Dialogs, ImgList, StdCtrls, ComCtrls, FunLabyUtils, GR32;

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
    procedure ListViewObjectsCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { D�clarations prives }
  public
    { D�clarations publiques }
    class procedure ShowObjects(Player: TPlayer);
  end;

implementation

{$R *.dfm}

{*
  Gestionnaire d'�v�nement OnCustomDrawItem de la liste
*}
procedure TFormObjects.ListViewObjectsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  TObjectDef(Item.Data).DrawIconToCanvas(Sender.Canvas,
    Item.DisplayRect(drIcon), TListView(Sender).Color);

  // Seems to be needed for correct drawing of the text
  Sender.Canvas.Brush.Style := bsClear;
end;

{*
  Affiche les objets d'un joueur
  @param Player   Joueur concern�
*}
class procedure TFormObjects.ShowObjects(Player: TPlayer);
var
  Master: TMaster;
  I: Integer;
  ObjectDef: TObjectDef;
  Infos: string;
begin
  with Create(Application) do
  try
    Master := Player.Master;

    for I := 0 to Master.ObjectDefCount-1 do
    begin
      ObjectDef := Master.ObjectDefs[I];
      Infos := ObjectDef.ShownInfos[Player];

      if Infos <> '' then
      begin
        with ListViewObjects.Items.Add do
        begin
          Caption := Infos;
          ImageIndex := 0;
          Data := Pointer(ObjectDef);
        end;
      end;
    end;

    ShowModal;
  finally
    Release;
  end;
end;

end.

