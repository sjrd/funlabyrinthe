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
  Dialogs, ImgList, StdCtrls, ComCtrls, CommCtrl, FunLabyUtils, GR32;

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
    procedure ListViewObjectsCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
  private
    { Déclarations prives }
    procedure AdjustColumnWidth;
  public
    { Déclarations publiques }
    class procedure ShowObjects(Player: TPlayer);
  end;

implementation

{$R *.dfm}

{*
  Gestionnaire d'événement OnCustomDrawItem de la liste
*}
procedure TFormObjects.ListViewObjectsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  TObjectDef(Item.Data).DrawIconToCanvas(Sender.Canvas,
    Item.DisplayRect(drIcon), TListView(Sender).Color);

  // Seems to be needed for correct drawing of the text
  SetBkMode(Sender.Canvas.Handle, TRANSPARENT);
end;

{*
  Ajuste la largeur de colonne de la liste d'objets
  Le calcul automatique fait par Windows ne prend pas en compte le fait que
  notre icône est plus grosse que prévue. Cette méthode corrige le tir.
*}
procedure TFormObjects.AdjustColumnWidth;
var
  ComputedWidth: Integer;
begin
  ListView_SetColumnWidth(ListViewObjects.Handle, 0, LVSCW_AUTOSIZE);
  ComputedWidth := ListView_GetColumnWidth(ListViewObjects.Handle, 0);
  ListView_SetColumnWidth(ListViewObjects.Handle, 0, ComputedWidth + 30);
end;

{*
  Affiche les objets d'un joueur
  @param Player   Joueur concerné
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

      if ObjectDef.ShouldDisplayInObjectList(Player, Infos) then
      begin
        with ListViewObjects.Items.Add do
        begin
          Caption := Infos;
          ImageIndex := 0;
          Data := Pointer(ObjectDef);
        end;
      end;
    end;

    AdjustColumnWidth;
    ShowModal;
  finally
    Release;
  end;
end;

end.

