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
  TObjectDefData = record
    ObjectDef: TObjectDef;
    Info: string;
  end;

  TObjectDefDataList = array of TObjectDefData;

  {*
    Boîte de dialogue affichant les objets possédés par un joueur
    @author sjrd
    @version 5.0
  *}
  TFormObjects = class(TForm)
    LabelObjects: TLabel;
    ListViewObjects: TListView;
    ButtonClose: TButton;
    ObjectsImages: TImageList;
    procedure FormShow(Sender: TObject);
    procedure ListViewObjectsCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ButtonCloseClick(Sender: TObject);
  private
    FPlayer: TPlayer; /// Joueur dont afficher les objets

    DisplayedData: TObjectDefDataList;
    ComputedData: TObjectDefDataList;

    procedure AdjustColumnWidth;
    procedure DoUpdateObjects;

    procedure ComputeData(Player: TPlayer);
    function IsDisplayedDataObsolete: Boolean;
    procedure UpdateDisplay;

    procedure SetPlayer(Value: TPlayer);
  public
    procedure UpdateObjects;

    property Player: TPlayer read FPlayer write SetPlayer;
  end;

implementation

{$R *.dfm}

{*
  Gestionnaire d'événement OnShow de la fiche
*}
procedure TFormObjects.FormShow(Sender: TObject);
begin
  DoUpdateObjects;
end;

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
  Gestionnaire d'événement OnClick du bouton Fermer
*}
procedure TFormObjects.ButtonCloseClick(Sender: TObject);
begin
  Close;
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
  Met à jour les données affichées
*}

procedure TFormObjects.DoUpdateObjects;
begin
  ComputeData(Player);
  if IsDisplayedDataObsolete then
    UpdateDisplay;
end;

{*
  Calcule ComputedData pour un joueur
  @param Player   Joueur concerné
*}
procedure TFormObjects.ComputeData(Player: TPlayer);
var
  Master: TMaster;
  I, Index: Integer;
  ObjectDef: TObjectDef;
  Infos: string;
begin
  ComputedData := nil;

  if Player = nil then
    Exit;

  Master := Player.Master;
  SetLength(ComputedData, Master.ObjectDefCount);

  Index := 0;

  for I := 0 to Master.ObjectDefCount-1 do
  begin
    ObjectDef := Master.ObjectDefs[I];

    if ObjectDef.ShouldDisplayInObjectList(Player, Infos) then
    begin
      ComputedData[Index].ObjectDef := ObjectDef;
      ComputedData[Index].Info := Infos;
      Inc(Index);
    end;
  end;

  SetLength(ComputedData, Index);
end;

{*
  Détermine si les données affichées sont obsolètes
  @return True si les données affichées doivent être rafraîchies
*}
function TFormObjects.IsDisplayedDataObsolete: Boolean;
var
  I: Integer;
begin
  Result := True;

  if Length(DisplayedData) <> Length(ComputedData) then
    Exit;

  for I := 0 to Length(DisplayedData)-1 do
  begin
    if DisplayedData[I].ObjectDef <> ComputedData[I].ObjectDef then
      Exit;
    if DisplayedData[I].Info <> ComputedData[I].Info then
      Exit;
  end;

  Result := False;
end;

{*
  Rafraîchit les données affichées
*}
procedure TFormObjects.UpdateDisplay;
var
  I: Integer;
begin
  ListViewObjects.Items.BeginUpdate;
  try
    DisplayedData := ComputedData;
    ListViewObjects.Items.Clear;

    for I := 0 to Length(DisplayedData)-1 do
    begin
      with ListViewObjects.Items.Add, DisplayedData[I] do
      begin
        Caption := Info;
        ImageIndex := 0;
        Data := Pointer(ObjectDef);
      end;
    end;
  finally
    ListViewObjects.Items.EndUpdate;
  end;

  AdjustColumnWidth;
end;

{*
  Modifie le joueur dont il faut afficher les données
  @param Value   Nouveau joueur (peut être nil)
*}
procedure TFormObjects.SetPlayer(Value: TPlayer);
begin
  if Value <> FPlayer then
  begin
    FPlayer := Value;
    UpdateObjects;
  end;
end;

{*
  Met à jour les données affichées
*}
procedure TFormObjects.UpdateObjects;
begin
  if Visible then
    DoUpdateObjects;
end;

end.

