{*
  Boîte de dialogue affichant et modifiant les plug-in d'un joueur
  L'unité PlayerAttributes propose une boîte de dialogue affichant les plug-in
  d'un joueur, et permettant de les modifier.
  @author sjrd
  @version 5.0
*}
unit PlayerPlugins;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Forms, Dialogs, Controls,
  StdCtrls, Buttons, FunLabyUtils;

type
  {*
    Boîte de dialogue affichant et modifiant les plug-in d'un joueur
    @author sjrd
    @version 5.0
  *}
  TFormPlugins = class(TForm)
    LabelAvailablePlugins: TLabel;
    ListBoxAvailablePlugins: TListBox;
    LabelAttachedPlugins: TLabel;
    ListBoxAttachedPlugins: TListBox;
    ButtonAttachPlugin: TSpeedButton;
    ButtonAttachAll: TSpeedButton;
    ButtonDetachPlugin: TSpeedButton;
    ButtonDetachAll: TSpeedButton;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    procedure ButtonAttachPluginClick(Sender: TObject);
    procedure ButtonDetachPluginClick(Sender: TObject);
    procedure ButtonAttachAllClick(Sender: TObject);
    procedure ButtonDetachAllClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure MoveSelected(List: TCustomListBox; Items: TStrings);
    procedure SetItem(List: TListBox; Index: Integer);
    function GetFirstSelection(List: TCustomListBox): Integer;
    procedure SetButtons;
  public
    { Déclarations publiques }
    class function ManagePlugins(Player: TPlayer): Boolean;
  end;

implementation

{$R *.dfm}

{*
  Déplace l'élément sélectionné dans une autre liste
  @param List    Boîte liste source
  @param Items   Liste d'éléments destination
*}
procedure TFormPlugins.MoveSelected(List: TCustomListBox; Items: TStrings);
var
  I: Integer;
begin
  for I := List.Items.Count-1 downto 0 do
  begin
    if List.Selected[I] then
    begin
      Items.AddObject(List.Items[I], List.Items.Objects[I]);
      List.Items.Delete(I);
    end;
  end;
end;

{*
  Met à jour la disponibilité des boutons
*}
procedure TFormPlugins.SetButtons;
var
  AvailableEmpty, AttachedEmpty: Boolean;
begin
  AvailableEmpty := ListBoxAvailablePlugins.Items.Count = 0;
  AttachedEmpty  := ListBoxAttachedPlugins .Items.Count = 0;

  ButtonAttachPlugin.Enabled := not AvailableEmpty;
  ButtonAttachAll   .Enabled := not AvailableEmpty;
  ButtonDetachPlugin.Enabled := not AttachedEmpty;
  ButtonDetachAll   .Enabled := not AttachedEmpty;
end;

{*
  Calcule l'index du premier élément sélectionné
  @param List   Boîte liste concernée
  @return L'index du premier élément sélectionné
*}
function TFormPlugins.GetFirstSelection(List: TCustomListBox): Integer;
begin
  for Result := 0 to List.Items.Count-1 do
    if List.Selected[Result] then
      Exit;
  Result := LB_ERR;
end;

{*
  Sélectionne un élément d'une boîte liste
  @param List    Boîte liste concernée
  @param Index   Index de l'élément à sélectionner
*}
procedure TFormPlugins.SetItem(List: TListBox; Index: Integer);
var
  MaxIndex: Integer;
begin
  with List do
  begin
    SetFocus;
    MaxIndex := List.Items.Count-1;

    if Index = LB_ERR then
      Index := 0
    else if Index > MaxIndex then
      Index := MaxIndex;
    Selected[Index] := True;
  end;

  SetButtons;
end;

{*
  Affiche et permet de modifier les plug-in attachés au joueur
  @param Player   Joueur concerné
  @return True si une modification a eu lieu, False sinon
*}
class function TFormPlugins.ManagePlugins(Player: TPlayer): Boolean;
var
  Master: TMaster;
  I: Integer;
begin
  with Create(Application) do
  try
    Master := Player.Master;

    Player.GetPluginIDs(ListBoxAttachedPlugins.Items);

    for I := 0 to Master.PluginCount-1 do
    begin
      if ListBoxAttachedPlugins.Items.IndexOf(Master.Plugins[I].ID) < 0 then
        ListBoxAvailablePlugins.Items.AddObject(
          Master.Plugins[I].ID, Master.Plugins[I]);
    end;

    SetButtons;

    if ShowModal <> mrOk then
      Result := False
    else
    begin
      for I := 0 to Master.PluginCount-1 do
        Player.RemovePlugin(Master.Plugins[I]);
      for I := 0 to ListBoxAttachedPlugins.Items.Count-1 do
        Player.AddPlugin(TPlugin(ListBoxAttachedPlugins.Items.Objects[I]));

      Result := True;
    end;
  finally
    Release;
  end;
end;

procedure TFormPlugins.ButtonAttachPluginClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(ListBoxAvailablePlugins);
  MoveSelected(ListBoxAvailablePlugins, ListBoxAttachedPlugins.Items);
  SetItem(ListBoxAvailablePlugins, Index);
end;

procedure TFormPlugins.ButtonDetachPluginClick(Sender: TObject);
var
  Index: Integer;
begin
  Index := GetFirstSelection(ListBoxAttachedPlugins);
  MoveSelected(ListBoxAttachedPlugins, ListBoxAvailablePlugins.Items);
  SetItem(ListBoxAttachedPlugins, Index);
end;

procedure TFormPlugins.ButtonAttachAllClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ListBoxAvailablePlugins.Items.Count-1 do
  begin
    ListBoxAttachedPlugins.Items.AddObject(
      ListBoxAvailablePlugins.Items[I],
      ListBoxAvailablePlugins.Items.Objects[I]);
  end;

  ListBoxAvailablePlugins.Items.Clear;
  SetItem(ListBoxAvailablePlugins, 0);
end;

procedure TFormPlugins.ButtonDetachAllClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to ListBoxAttachedPlugins.Items.Count-1 do
  begin
    ListBoxAvailablePlugins.Items.AddObject(
      ListBoxAttachedPlugins.Items[I],
      ListBoxAttachedPlugins.Items.Objects[I]);
  end;

  ListBoxAttachedPlugins.Items.Clear;
  SetItem(ListBoxAttachedPlugins, 0);
end;

end.

