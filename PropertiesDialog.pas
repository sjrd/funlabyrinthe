unit PropertiesDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FunLabyUtils;

resourcestring
  sPlayerProperties = 'Propriétés du joueur';
  sPlayerObjects = 'Objets possédés par le joueur :';
  sMapProperties = 'Propriétés de la carte';

type
  TFormProperties = class(TForm)
    ListBox: TListBox;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    procedure ShowPlayerProps(Player : TPlayer);
    procedure ShowMapProps(Map : TMap);
  end;

var
  FormProperties: TFormProperties;

implementation

{$R *.DFM}

procedure TFormProperties.ShowPlayerProps(Player : TPlayer);
var I : integer;
begin
  with ListBox.Items do
  begin
    Caption := sPlayerProperties;
    Clear;

    Add(sPlayerObjects);
    Add('');

    for I := 0 to Player.Master.ObjectDefCount-1 do
      Player.Master.ObjectDefs[I].ShownInfos[Player];

    ShowModal;
  end;
end;

procedure TFormProperties.ShowMapProps(Map : TMap);
begin
  with ListBox.Items do
  begin
    Caption := sMapProperties;
    Clear;

    { TODO 1 : Afficher les propriétés de la carte }

    ShowModal;
  end;
end;

end.
